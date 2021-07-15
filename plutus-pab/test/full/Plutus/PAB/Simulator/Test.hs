{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TypeApplications #-}
{-

A 'Simulator' for the test contracts

-}
module Plutus.PAB.Simulator.Test(runSimulation) where

import           Control.Monad.Freer                      (interpret)
import           Plutus.PAB.Core                          (EffectHandlers)
import           Plutus.PAB.Effects.Contract.Builtin      (Builtin, BuiltinHandler (contractHandler), handleBuiltin)
import           Plutus.PAB.Effects.Contract.ContractTest (TestContracts (..))
import           Plutus.PAB.Simulator                     (Simulation, SimulatorContractHandler, SimulatorState,
                                                           mkSimulatorHandlers, runSimulationWith)
import           Plutus.PAB.Types                         (PABError)

-- | 'EffectHandlers' for running the PAB as a simulator (no connectivity to
--   out-of-process services such as wallet backend, node, etc.)
simulatorHandlers :: EffectHandlers (Builtin TestContracts) (SimulatorState (Builtin TestContracts))
simulatorHandlers = mkSimulatorHandlers @(Builtin TestContracts) handler where
    handler :: SimulatorContractHandler (Builtin TestContracts)
    handler = interpret (contractHandler (handleBuiltin @TestContracts))

-- | Run the PAB simulator with the test contracts
runSimulation :: Simulation (Builtin TestContracts) a -> IO (Either PABError a)
runSimulation = runSimulationWith @(Builtin TestContracts) simulatorHandlers
