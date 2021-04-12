{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Debug.LiveCoding (runSession) where

import           Control.Concurrent           (threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Data.List.Split              (splitOn)
import qualified DynFlags                     as F
import qualified EnumSet
import qualified GHC
import           GHC.Paths                    (libdir)
import           Graphics.GPipe.Engine.TimeIt (Info (..), Status (Done, Fail),
                                               timeIt)
import           System.Directory             (setCurrentDirectory)
import           System.Environment           as Env (getArgs)
import           System.FilePath              ((</>))


instance Info GHC.SuccessFlag where
    getInfo f@GHC.Succeeded = (f, (Done, "OK"))
    getInfo f@GHC.Failed    = (f, (Fail, "Failed"))


runExpr :: GHC.GhcMonad m => GHC.ModSummary -> String -> m Bool
runExpr modSum expr = do
    GHC.setContext [GHC.IIDecl . GHC.simpleImportDecl . GHC.moduleName . GHC.ms_mod $ modSum]
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


interpret :: String -> [String] -> IO ()
interpret modName pkgs = GHC.runGhc (Just libdir) $ do
    let modPath = foldl (</>) "src" (splitOn "." modName) ++ ".hs"
    dflags <- GHC.getSessionDynFlags
    _ <- GHC.setSessionDynFlags (dflags
        { GHC.ghcLink = GHC.LinkInMemory
        , GHC.hscTarget = GHC.HscAsm
        -- TODO: figure out how to make this work with HscAsm.
        -- It works with HscInterpreted, but with HscAsm I get an "impossible"
        -- error and crash.
        -- , GHC.generalFlags = EnumSet.fromList [F.Opt_HideAllPackages]
        , GHC.packageFlags =
            [ F.ExposePackage pkg (F.PackageArg pkg) (F.ModRenaming True [])
            | pkg <- "base" : pkgs]
        -- C:\\Users\\Pippijn\\AppData\\Roaming\\cabal\\store\\ghc-8.10.4\\package.db
        -- C:\\Users\\Pippijn\\Documents\\code\\lambdaray\\dist-newstyle\\build\\x86_64-windows\\ghc-8.10.4\\lambdaray-0.0.1\\package.conf.inplace
        -- C:\\Users\\Pippijn\\Documents\\code\\lambdaray\\dist-newstyle\\packagedb\\ghc-8.10.4
        , GHC.packageDBFlags =
            [ F.PackageDB (F.PkgConfFile db)
            | db <- [ "C:\\Users\\Pippijn\\Documents\\code\\workspace\\dist-newstyle\\packagedb\\ghc-8.10.4"
                    , "C:\\Users\\Pippijn\\AppData\\Roaming\\cabal\\store\\ghc-8.10.4\\package.db"
                    ]
            ]
        , GHC.importPaths = ["src"]
        })
    target <- GHC.guessTarget modPath Nothing
    GHC.setTargets [target]
    whileM $ do
        ok <- timeIt ("Compiling modules for " ++ modName) $ GHC.load GHC.LoadAllTargets
        case ok of
            GHC.Succeeded -> do
                modSum <- GHC.getModSummary $ GHC.mkModuleName $ modName
                runExpr modSum "main"
            GHC.Failed -> do
                liftIO $ threadDelay 1000000
                return True


runSession :: FilePath -> IO () -> String -> [String] -> IO ()
runSession dir static modName pkgs = do
    setCurrentDirectory dir
    Env.getArgs >>= \case
        args | all (`elem` args) ["-static"] -> static
        _                                    -> interpret modName pkgs
