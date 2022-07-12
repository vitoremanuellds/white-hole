{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_white_hole (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/vitor/estudos/plp/white-hole/white-hole/.stack-work/install/x86_64-linux-tinfo6/d8bd72789c803ea1187df76288cc753ac8f6cb6bd06ce25e89457e9ebaeebfaf/9.0.2/bin"
libdir     = "/home/vitor/estudos/plp/white-hole/white-hole/.stack-work/install/x86_64-linux-tinfo6/d8bd72789c803ea1187df76288cc753ac8f6cb6bd06ce25e89457e9ebaeebfaf/9.0.2/lib/x86_64-linux-ghc-9.0.2/white-hole-0.1.0.0-EuUnoSHcW7s7XVTZxJiRYH-white-hole"
dynlibdir  = "/home/vitor/estudos/plp/white-hole/white-hole/.stack-work/install/x86_64-linux-tinfo6/d8bd72789c803ea1187df76288cc753ac8f6cb6bd06ce25e89457e9ebaeebfaf/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/vitor/estudos/plp/white-hole/white-hole/.stack-work/install/x86_64-linux-tinfo6/d8bd72789c803ea1187df76288cc753ac8f6cb6bd06ce25e89457e9ebaeebfaf/9.0.2/share/x86_64-linux-ghc-9.0.2/white-hole-0.1.0.0"
libexecdir = "/home/vitor/estudos/plp/white-hole/white-hole/.stack-work/install/x86_64-linux-tinfo6/d8bd72789c803ea1187df76288cc753ac8f6cb6bd06ce25e89457e9ebaeebfaf/9.0.2/libexec/x86_64-linux-ghc-9.0.2/white-hole-0.1.0.0"
sysconfdir = "/home/vitor/estudos/plp/white-hole/white-hole/.stack-work/install/x86_64-linux-tinfo6/d8bd72789c803ea1187df76288cc753ac8f6cb6bd06ce25e89457e9ebaeebfaf/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "white_hole_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "white_hole_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "white_hole_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "white_hole_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "white_hole_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "white_hole_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
