{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Control.Arrow ((***))
import Data.Monoid
import Data.IORef
import Text.Printf
import System.Posix
import System.Environment
import System.FilePath
import System.IO
import System.Linux.FileExtents
import System.Console.ANSI

import System.Linux.Btrfs

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    paths <- getArgs
    statsRef <- newIORef mempty
    printStats mempty
    mapM_ (flip traverseTree (defragFile statsRef)) paths
    putChar '\n'

defragFile :: IORef Stats -> FilePath -> FileStatus -> IO ()
defragFile statsRef path stat
    | isRegularFile stat = do
        extBefore <- getExtentCount defReqFlags path Nothing
        when (extBefore > 1) $ do -- skip files with 1 extent
            handleIOExn $ defragRange path dra
            extAfter  <- getExtentCount defReqFlags path Nothing
            stats <- readIORef statsRef
            let stats' = stats <> Stats
                    { stFiles = 1
                    , stBytes = fromIntegral (fileSize stat)
                    , stExtentsBefore = fromIntegral extBefore
                    , stExtentsAfter  = fromIntegral extAfter
                    }
            writeIORef statsRef stats'
            printStats stats'
    | otherwise = return ()
  where
    dra = defaultDefragRangeArgs
        { draExtentThreshold = 32 * 1024 * 1024
        , draFlush = True
        }

traverseTree :: FilePath -> (FilePath -> FileStatus -> IO ()) -> IO ()
traverseTree path action = do
    stat <- getSymbolicLinkStatus path
    action path stat
    when (isDirectory stat) $
        loopDir path $ \s ->
            traverseTree (path </> s) action

loopDir :: FilePath -> (FilePath -> IO ()) -> IO ()
loopDir path action =
    bracket (openDirStream path) closeDirStream $ \dir ->
        fix $ \loop -> do
            s <- readDirStream dir
            unless (null s) $ do
                unless (s == "." || s == "..") $
                    action s
                loop

handleIOExn :: IO () -> IO ()
handleIOExn =
    handle $ \e -> do
        putChar '\r'
        clearFromCursorToLineEnd
        hFlush stdout
        hPrint stderr (e :: IOException)

data Stats = Stats
    { stFiles :: !Int
    , stBytes :: !Integer
    , stExtentsBefore :: !Int
    , stExtentsAfter  :: !Int
    }

instance Monoid Stats where
    mempty = Stats 0 0 0 0
    mappend (Stats a1 b1 c1 d1) (Stats a2 b2 c2 d2) =
        Stats (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

printStats :: Stats -> IO ()
printStats Stats{..} = do
    printf "\rprocessed: %d file(s)/%s, extents (before/after): %d/%d"
        stFiles
        (prettyFileSize stBytes)
        stExtentsBefore
        stExtentsAfter
    clearFromCursorToLineEnd
    hFlush stdout

prettyFileSize :: Integer -> String
prettyFileSize s
    | s < 1024 = printf "%d b" s
    | s' < 10   = printf "%.2f %ciB" s' c
    | s' < 100  = printf "%.1f %ciB" s' c
    | otherwise = printf "%.0f %ciB" s' c
  where
    (s', c : _) = until ((< 1024) . fst)
                        ((/ 1024) *** tail)
                        (fromIntegral s / 1024 :: Double, "KMGTPEZY")
