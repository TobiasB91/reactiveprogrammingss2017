{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_curried_in_space (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tobias/Dropbox/Universita\776t/Reactive/ueb4/.stack-work/install/x86_64-osx/lts-8.4/8.0.2/bin"
libdir     = "/Users/tobias/Dropbox/Universita\776t/Reactive/ueb4/.stack-work/install/x86_64-osx/lts-8.4/8.0.2/lib/x86_64-osx-ghc-8.0.2/curried-in-space-0.1"
dynlibdir  = "/Users/tobias/Dropbox/Universita\776t/Reactive/ueb4/.stack-work/install/x86_64-osx/lts-8.4/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/tobias/Dropbox/Universita\776t/Reactive/ueb4/.stack-work/install/x86_64-osx/lts-8.4/8.0.2/share/x86_64-osx-ghc-8.0.2/curried-in-space-0.1"
libexecdir = "/Users/tobias/Dropbox/Universita\776t/Reactive/ueb4/.stack-work/install/x86_64-osx/lts-8.4/8.0.2/libexec"
sysconfdir = "/Users/tobias/Dropbox/Universita\776t/Reactive/ueb4/.stack-work/install/x86_64-osx/lts-8.4/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "curried_in_space_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "curried_in_space_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "curried_in_space_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "curried_in_space_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "curried_in_space_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "curried_in_space_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
