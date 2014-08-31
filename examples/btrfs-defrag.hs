import Control.Monad
import Control.Monad.Fix
import Control.Exception
import System.Posix
import System.Environment
import System.FilePath
import System.IO

import System.Linux.Btrfs

main :: IO ()
main = do
    paths <- getArgs
    mapM_ defragRec paths

defragRec :: FilePath -> IO ()
defragRec path0 =
    traverseTree path0 $ \path stat ->
        when (isRegularFile stat) $
            handleIOExn $ defrag path

traverseTree :: FilePath -> (FilePath -> FileStatus -> IO ()) -> IO ()
traverseTree path action = do
    stat <- getSymbolicLinkStatus path
    action path stat
    when (isDirectory stat) $ do
        loopDir path $ \s ->
            traverseTree (path </> s) action

loopDir :: FilePath -> (FilePath -> IO ()) -> IO ()
loopDir path action = do
    bracket (openDirStream path) closeDirStream $ \dir ->
        fix $ \loop -> do
            s <- readDirStream dir
            unless (null s) $ do
                unless (s == "." || s == "..") $ action s
                loop

handleIOExn :: IO () -> IO ()
handleIOExn =
    handle (\e -> hPrint stderr (e ::  IOException))
