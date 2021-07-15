module Dashboard.Lenses
  ( _walletDataState
  , _walletDetails
  , _menuOpen
  , _card
  , _cardOpen
  , _contracts
  , _contractFilter
  , _selectedContractIndex
  , _selectedContract
  , _templateState
  ) where

import Prelude
import Contract.Types (State) as Contract
import Dashboard.Types (Card, ContractFilter, State)
import Data.Lens (Lens', Traversal', set, wander)
import Data.Lens.Record (prop)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Marlowe.PAB (PlutusAppId)
import Template.Types (State) as Template
import WalletData.Types (State) as WalletData
import WalletData.Types (WalletDetails)

_walletDataState :: Lens' State WalletData.State
_walletDataState = prop (SProxy :: SProxy "walletDataState")

_walletDetails :: Lens' State WalletDetails
_walletDetails = prop (SProxy :: SProxy "walletDetails")

_menuOpen :: Lens' State Boolean
_menuOpen = prop (SProxy :: SProxy "menuOpen")

_card :: Lens' State (Maybe Card)
_card = prop (SProxy :: SProxy "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (SProxy :: SProxy "cardOpen")

_contracts :: Lens' State (Map PlutusAppId Contract.State)
_contracts = prop (SProxy :: SProxy "contracts")

_contractFilter :: Lens' State ContractFilter
_contractFilter = prop (SProxy :: SProxy "contractFilter")

_selectedContractIndex :: Lens' State (Maybe PlutusAppId)
_selectedContractIndex = prop (SProxy :: SProxy "selectedContractIndex")

-- This traversal focus on a specific contract indexed by another property of the state
_selectedContract :: Traversal' State Contract.State
_selectedContract =
  wander \f state -> case state.selectedContractIndex of
    Just ix
      | Just contract <- lookup ix state.contracts ->
        let
          updateContract contract' = insert ix contract' state.contracts
        in
          (\contract' -> set _contracts (updateContract contract') state) <$> f contract
    _ -> pure state

_templateState :: Lens' State Template.State
_templateState = prop (SProxy :: SProxy "templateState")
