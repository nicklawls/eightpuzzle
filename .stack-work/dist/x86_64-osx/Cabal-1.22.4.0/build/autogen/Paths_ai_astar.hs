module Paths_ai_astar (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nicklawler222/Code/ai-astar/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/bin"
libdir     = "/Users/nicklawler222/Code/ai-astar/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/lib/x86_64-osx-ghc-7.10.2/ai-astar-0.1.0.0-9KQGwubUFpm1CruK3v3qIU"
datadir    = "/Users/nicklawler222/Code/ai-astar/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/share/x86_64-osx-ghc-7.10.2/ai-astar-0.1.0.0"
libexecdir = "/Users/nicklawler222/Code/ai-astar/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/libexec"
sysconfdir = "/Users/nicklawler222/Code/ai-astar/.stack-work/install/x86_64-osx/lts-3.9/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ai_astar_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ai_astar_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ai_astar_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ai_astar_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ai_astar_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
