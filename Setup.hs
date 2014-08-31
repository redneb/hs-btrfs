import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import System.IO
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {buildHook = customBuildHook}

customBuildHook desc lbi hooks flags = do
    let verbosity = fromFlag (buildVerbosity flags)

    -- create System/Linux/Btrfs/ByteString.hsc
    let dir = autogenModulesDir lbi </> "System/Linux/Btrfs"
    createDirectoryIfMissingVerbose verbosity True dir
    withFile (dir </> "ByteString.hsc") WriteMode $ \oHdl -> do
        hPutStrLn oHdl "#define BTRFS_RAW_PATHS 1"
        withFile "System/Linux/Btrfs.hsc" ReadMode $ \iHdl -> do
            _ <- hGetLine iHdl -- skip the first line
            contents <- hGetContents iHdl
            hPutStr oHdl contents

    buildHook simpleUserHooks desc lbi hooks flags
