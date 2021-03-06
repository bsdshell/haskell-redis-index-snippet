{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_redis_index_snippet (
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

bindir     = "/Users/aaa/myfile/github/haskell-redis-index-snippet/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/bin"
libdir     = "/Users/aaa/myfile/github/haskell-redis-index-snippet/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/lib/x86_64-osx-ghc-8.6.5/haskell-redis-index-snippet-0.1.0.0-Fag2fUbVyCaAfQu0QXt7Hq-haskell-redis-index-snippet"
dynlibdir  = "/Users/aaa/myfile/github/haskell-redis-index-snippet/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/aaa/myfile/github/haskell-redis-index-snippet/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/share/x86_64-osx-ghc-8.6.5/haskell-redis-index-snippet-0.1.0.0"
libexecdir = "/Users/aaa/myfile/github/haskell-redis-index-snippet/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/libexec/x86_64-osx-ghc-8.6.5/haskell-redis-index-snippet-0.1.0.0"
sysconfdir = "/Users/aaa/myfile/github/haskell-redis-index-snippet/.stack-work/install/x86_64-osx/e4a1e3da1599055cfb106fd2d2765c8f6797bcd3520e32d5e09fa0cf6073c2a2/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_redis_index_snippet_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_redis_index_snippet_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_redis_index_snippet_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_redis_index_snippet_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_redis_index_snippet_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_redis_index_snippet_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
