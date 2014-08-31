import System.Environment

import System.Linux.Btrfs

main :: IO ()
main = do
    [srcPath, srcOff, srcLen, dstPath, dstOff] <- getArgs
    cloneRange srcPath (read srcOff) (read srcLen)
               dstPath (read dstOff)
