{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE EmptyDataDeriving   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-

Builtin contracts that are compiled together with the PAB.

-}
module Plutus.PAB.Effects.Contract.Builtin(
    Builtin
    , ContractConstraints
    , SomeBuiltin(..)
    , SomeBuiltinState(..)
    , BuiltinContract(..)
    , BuiltinHandler(..)
    , handleBuiltin
    -- * Extracting schemas from contracts
    , type (.\\)
    , type (.\/)
    , EmptySchema
    , Empty
    , endpointsToSchemas
    , getResponse
    , fromResponse
    , HasDefinitions(..)
    , initBuiltin
    , updateBuiltin
    ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error                        (Error, throwError)
import           Control.Monad.Freer.Extras.Log                   (LogMsg (..), logDebug)
import           Data.Aeson                                       (FromJSON, ToJSON, Value)
import qualified Data.Aeson                                       as JSON
import           Data.Bifunctor                                   (Bifunctor (first))
import           Data.Foldable                                    (traverse_)
import           Data.Row

import           Plutus.Contract.Effects                          (PABReq, PABResp)
import           Plutus.Contract.Types                            (ResumableResult (..), SuspendedContract (..))
import           Plutus.PAB.Effects.Contract                      (ContractEffect (..), PABContract (..))
import           Plutus.PAB.Monitoring.PABLogMsg                  (PABMultiAgentMsg (..))
import           Plutus.PAB.Types                                 (PABError (..))

import           GHC.Generics                                     (Generic)
import           Playground.Schema                                (endpointsToSchemas)
import           Playground.Types                                 (FunctionSchema)
import           Plutus.Contract                                  (Contract, ContractInstanceId, EmptySchema)
import           Plutus.Contract.Resumable                        (Response)
import           Plutus.Contract.Schema                           (Input, Output)
import           Plutus.Contract.State                            (ContractResponse (..))
import qualified Plutus.Contract.State                            as ContractState
import           Plutus.PAB.Core.ContractInstance.RequestHandlers (ContractInstanceMsg (ContractLog, ProcessFirstInboxMessage))
import           Plutus.Trace.Emulator.Types                      (ContractInstanceStateInternal (..))
import qualified Plutus.Trace.Emulator.Types                      as Emulator
import           Schema                                           (FormSchema)

-- | Contracts that are built into the PAB (ie. compiled with it) and receive
--   an initial value of type 'a'.
--
-- We have a dummy constructor so that we can convert this datatype in
-- Purescript with '(equal <*> (genericShow <*> mkSumType)) (Proxy @(Builtin A))'.
data Builtin a = Builtin deriving (Eq, Generic)

type ContractConstraints w schema error =
    ( Monoid w
    , Forall (Output schema) ToJSON
    , Forall (Input schema) ToJSON
    , Forall (Input schema) FromJSON
    , ToJSON error
    , ToJSON w
    , FromJSON w
    , AllUniqueLabels (Input schema)
    )

-- | Plutus contract with all parameters existentially quantified. Can be any contract that satisfies the
--   'ContractConstraints'.
data SomeBuiltin where
    SomeBuiltin :: forall w schema error a. ContractConstraints w schema error => Contract w schema error a -> SomeBuiltin

data SomeBuiltinState a where
    SomeBuiltinState ::
        forall a w schema error b.
        ContractConstraints w schema error
        => Emulator.ContractInstanceStateInternal w schema error b -- ^ Internal state
        -> w -- ^ Observable state (stored separately)
        -> SomeBuiltinState a

instance PABContract (Builtin a) where
    type ContractDef (Builtin a) = a
    type State (Builtin a) = SomeBuiltinState a
    serialisableState _ = getResponse

class HasDefinitions a where
    getDefinitions :: [a]
    getContract :: a -> SomeBuiltin -- ^ The actual contract
    getSchema :: a -> [FunctionSchema FormSchema] -- ^ The schema (construct with 'endpointsToSchemas'. Can also be an empty list)

-- | Defined in order to prevent type errors like: "Couldn't match type 'effs'
-- with 'effs1'".
newtype BuiltinHandler a = BuiltinHandler
    { contractHandler :: forall effs.
                         ( Member (Error PABError) effs
                         , Member (LogMsg (PABMultiAgentMsg (Builtin a))) effs
                         )
                      => ContractEffect (Builtin a) ~> Eff effs
    }

-- | Handle the 'ContractEffect' for a builtin contract type with parameter
--   @a@.
handleBuiltin :: HasDefinitions a => BuiltinHandler a
handleBuiltin = BuiltinHandler $ \case
    InitialState i c           -> case getContract c of SomeBuiltin c' -> initBuiltin i c'
    UpdateContract i _ state p -> case state of SomeBuiltinState s w -> updateBuiltin i s w p
    ExportSchema a             -> pure $ getSchema a

getResponse :: forall a. SomeBuiltinState a -> ContractResponse Value Value Value PABReq
getResponse (SomeBuiltinState s w) =
    first JSON.toJSON
    $ ContractState.mapE JSON.toJSON
    $ ContractState.mapW JSON.toJSON
    $ ContractState.mkResponse w
    $ Emulator.instContractState
    $ Emulator.toInstanceState s

-- | TODO: Replay from the ContractResponse the state of 'Builtin a' using 'Emulator.emptyInstanceState' and 'Emulator.addEventInstanceState'.
fromResponse :: forall a. ContractResponse Value Value Value PABReq -> Maybe (SomeBuiltinState a)
fromResponse _ = do
    -- let toPABResp = \x -> JSON.fromJSON x :: Result PABResp
    -- let resp' = fmap (toPABResp . snd) <$> responses (record $ newState resp)
    -- w <- case JSON.fromJSON lastState of
    --        Error _ -> Nothing
    --        Success w -> pure w
    -- pure $ SomeBuiltinState undefined w
    pure undefined
 -- where
 --     toPABResp :: Value -> Maybe PABResp
 --     toPABResp v =
 --         case JSON.fromJSON v of
 --           Success v' -> Just v'
 --           Error _ -> Nothing

initBuiltin ::
    forall effs a w schema error b.
    ( ContractConstraints w schema error
    , Member (LogMsg (PABMultiAgentMsg (Builtin a))) effs
    )
    => ContractInstanceId
    -> Contract w schema error b
    -> Eff effs (SomeBuiltinState a)
initBuiltin i con = do
    let initialState = Emulator.emptyInstanceState con
    logNewMessages @a i initialState
    pure $ SomeBuiltinState initialState mempty

updateBuiltin ::
    forall effs a w schema error b.
    ( ContractConstraints w schema error
    , Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin a))) effs
    )
    => ContractInstanceId
    -> Emulator.ContractInstanceStateInternal w schema error b
    -> w
    -> Response PABResp
    -> Eff effs (SomeBuiltinState a)
updateBuiltin i oldState oldW resp = do
    let newState = Emulator.addEventInstanceState resp oldState
    case newState of
        Just k -> do
            logDebug @(PABMultiAgentMsg (Builtin a)) (ContractInstanceLog $ ProcessFirstInboxMessage i resp)
            logNewMessages @a i k
            let newW = oldW <> (_lastState $ _resumableResult $ Emulator.cisiSuspState oldState)
            pure (SomeBuiltinState k newW)
        _      -> throwError $ ContractCommandError 0 "failed to update contract"

logNewMessages ::
    forall b w s e a effs.
    ( Member (LogMsg (PABMultiAgentMsg (Builtin b))) effs
    )
    => ContractInstanceId
    -> ContractInstanceStateInternal w s e a
    -> Eff effs ()
logNewMessages i ContractInstanceStateInternal{cisiSuspState=SuspendedContract{_resumableResult=ResumableResult{_lastLogs, _observableState}}} = do
    traverse_ (send @(LogMsg (PABMultiAgentMsg (Builtin b))) . LMessage . fmap (ContractInstanceLog . ContractLog i)) _lastLogs
