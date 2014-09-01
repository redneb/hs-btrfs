#!/usr/bin/runhaskell

import System.Directory
import System.IO
import Control.Monad

main :: IO ()
main = do
    doesFileExist "btrfs.cabal" >>= \f ->
        unless f $ fail "could not find btrfs.cabal"

    -- create System/Linux/Btrfs/ByteString.hsc
    withFile "System/Linux/Btrfs/ByteString.hsc" WriteMode $ \oHdl -> do
        hPutStrLn oHdl "#define BTRFS_RAW_PATHS 1"
        withFile "System/Linux/Btrfs.hsc" ReadMode $ \iHdl -> do
            _ <- hGetLine iHdl -- skip the first line
            contents <- hGetContents iHdl
            hPutStr oHdl contents
