{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import qualified DynFlags               as F
import qualified GHC
import           GHC.Paths              (libdir)
import           System.Directory       (setCurrentDirectory)
import           System.Environment     as Env (getArgs)

import qualified LambdaRay.GPU          as GPU


runExpr :: GHC.GhcMonad m => GHC.ModSummary -> String -> m Bool
runExpr modSum expr = do
    GHC.setContext [GHC.IIModule $ GHC.moduleName $ GHC.ms_mod modSum]
    rr <- GHC.execStmt expr GHC.execOptions
    case rr of
        GHC.ExecComplete{GHC.execResult=Right _}  -> return True
        GHC.ExecComplete{GHC.execResult=Left err} -> do
            liftIO $ print err
            return False
        _                                         -> return False


whileM :: Monad m => m Bool -> m ()
whileM a = do
    r <- a
    when r $ whileM a


interpret :: String -> IO ()
interpret modName = GHC.runGhc (Just libdir) $ do
    dflags <- GHC.getSessionDynFlags
    _ <- GHC.setSessionDynFlags (dflags
        { GHC.ghcLink = GHC.LinkInMemory
        , GHC.hscTarget = GHC.HscInterpreted
        , GHC.packageFlags =
            [ F.ExposePackage pkg (F.PackageArg pkg) (F.ModRenaming True [])
            | pkg <- ["GPipe-Core", "GPipe-GLFW", "JuicyPixels", "lens", "time"]
            ] ++
            [ F.HidePackage pkg
            | pkg <- ["GPipe", "GPipe-Base"]
            ]
        -- C:\\Users\\Pippijn\\AppData\\Roaming\\cabal\\store\\ghc-8.10.4\\package.db
        -- C:\\Users\\Pippijn\\Documents\\code\\lambdaray\\dist-newstyle\\build\\x86_64-windows\\ghc-8.10.4\\lambdaray-0.0.1\\package.conf.inplace
        -- C:\\Users\\Pippijn\\Documents\\code\\lambdaray\\dist-newstyle\\packagedb\\ghc-8.10.4
        , GHC.packageDBFlags =
            [ F.PackageDB (F.PkgConfFile db)
            | db <- [ "C:\\Users\\Pippijn\\Documents\\code\\lambdaray\\dist-newstyle\\packagedb\\ghc-8.10.4"
                    , "C:\\Users\\Pippijn\\AppData\\Roaming\\cabal\\store\\ghc-8.10.4\\package.db"
                    ]
            ]
        , GHC.importPaths = ["src"]
        })
    target <- GHC.guessTarget ("src/LambdaRay/" ++ modName ++ ".hs") Nothing
    GHC.setTargets [target]
    whileM $ do
        ok <- GHC.load GHC.LoadAllTargets
        case ok of
            GHC.Succeeded -> do
                modSum <- GHC.getModSummary $ GHC.mkModuleName $ "LambdaRay." ++ modName
                runExpr modSum "main"
            GHC.Failed -> do
                liftIO $ threadDelay 1000000
                return True


main :: IO ()
main = do
    setCurrentDirectory "lambdaray"
    Env.getArgs >>= \case
        args | all (`elem` args) ["-static"] -> GPU.main
        _                                    -> interpret "GPU"
