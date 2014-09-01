import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.ModuleName (fromString)
import System.IO
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { confHook = customConf
    , buildHook = customBuild
    }

customConf gh cFlags = do
    lbi <- confHook simpleUserHooks gh cFlags
    -- modify the package description to add the auto-generated module
    let desc = localPkgDescr lbi
        Just lib = library desc
        modName = fromString "System.Linux.Btrfs.ByteString"
        lib' = lib {exposedModules = exposedModules lib ++ [modName]}
        desc' = desc {library = Just lib'}
    return lbi {localPkgDescr = desc'}

customBuild desc lbi hooks bFlags = do
    let verbosity = fromFlag (buildVerbosity bFlags)

    -- create System/Linux/Btrfs/ByteString.hsc
    let dir = autogenModulesDir lbi </> "System/Linux/Btrfs"
    createDirectoryIfMissingVerbose verbosity True dir
    withFile (dir </> "ByteString.hsc") WriteMode $ \oHdl -> do
        hPutStrLn oHdl "#define BTRFS_RAW_PATHS 1"
        withFile "System/Linux/Btrfs.hsc" ReadMode $ \iHdl -> do
            _ <- hGetLine iHdl -- skip the first line
            contents <- hGetContents iHdl
            hPutStr oHdl contents

    buildHook simpleUserHooks desc lbi hooks bFlags
