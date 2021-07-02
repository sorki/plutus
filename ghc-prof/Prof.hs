module Main where

-- import Lib
import           GHC                    as G
-- import GHC.Driver.Session as G
-- import GHC.Driver.Session
import           SrcLoc                 as G

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Outputable
import           System.Environment
import           System.Mem

initGhcM :: [String] -> Ghc ()
initGhcM xs = do
    df1 <- getSessionDynFlags
    let cmdOpts = ["-fforce-recomp"] ++ xs
    (df2, leftovers, warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
    setSessionDynFlags df2
    ts <- mapM (flip G.guessTarget Nothing) $ map unLoc leftovers
    setTargets ts
    pprTraceM "Starting" (ppr ts)
    void $ G.load LoadAllTargets

main :: IO ()
main = do
    xs <- words <$> readFile "args"
    let libdir = "/nix/store/lwkk0g84hw4yfy19fihl295hprbpf060-ghc-shell-for-packages-ghc-8.10.4.20210212-env/lib/ghc-8.10.4.20210212"
    runGhc (Just libdir) $ initGhcM xs
