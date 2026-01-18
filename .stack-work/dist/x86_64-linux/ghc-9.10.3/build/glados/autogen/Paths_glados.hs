{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_glados (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/woodpecker/src/github.com/CorpsB/Glados/.stack-work/install/x86_64-linux/7da536dd1b9327dace3b2b3b96d15870fbbb66514931a7edf7d80c72d55b381b/9.10.3/bin"
libdir     = "/woodpecker/src/github.com/CorpsB/Glados/.stack-work/install/x86_64-linux/7da536dd1b9327dace3b2b3b96d15870fbbb66514931a7edf7d80c72d55b381b/9.10.3/lib/x86_64-linux-ghc-9.10.3-5156/glados-0.1.0.0-65j86GrkW72HwhqMOvC3na-glados"
dynlibdir  = "/woodpecker/src/github.com/CorpsB/Glados/.stack-work/install/x86_64-linux/7da536dd1b9327dace3b2b3b96d15870fbbb66514931a7edf7d80c72d55b381b/9.10.3/lib/x86_64-linux-ghc-9.10.3-5156"
datadir    = "/woodpecker/src/github.com/CorpsB/Glados/.stack-work/install/x86_64-linux/7da536dd1b9327dace3b2b3b96d15870fbbb66514931a7edf7d80c72d55b381b/9.10.3/share/x86_64-linux-ghc-9.10.3-5156/glados-0.1.0.0"
libexecdir = "/woodpecker/src/github.com/CorpsB/Glados/.stack-work/install/x86_64-linux/7da536dd1b9327dace3b2b3b96d15870fbbb66514931a7edf7d80c72d55b381b/9.10.3/libexec/x86_64-linux-ghc-9.10.3-5156/glados-0.1.0.0"
sysconfdir = "/woodpecker/src/github.com/CorpsB/Glados/.stack-work/install/x86_64-linux/7da536dd1b9327dace3b2b3b96d15870fbbb66514931a7edf7d80c72d55b381b/9.10.3/etc"

getBinDir     = catchIO (getEnv "glados_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "glados_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "glados_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "glados_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "glados_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "glados_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
