module Paths_Haskell1 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/test/Desktop/Haskell1/.cabal-sandbox/bin"
libdir     = "/Users/test/Desktop/Haskell1/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/Haskell1-0.1.0.0"
datadir    = "/Users/test/Desktop/Haskell1/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/Haskell1-0.1.0.0"
libexecdir = "/Users/test/Desktop/Haskell1/.cabal-sandbox/libexec"
sysconfdir = "/Users/test/Desktop/Haskell1/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Haskell1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Haskell1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Haskell1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskell1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskell1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
