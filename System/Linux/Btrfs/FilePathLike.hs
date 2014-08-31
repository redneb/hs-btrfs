{-# LANGUAGE FlexibleInstances #-}

module System.Linux.Btrfs.FilePathLike
    ( FilePathLike(..)
    , RawFilePath
    ) where

import Data.String
import Data.Monoid
import Data.List
import Foreign.C.String hiding
    ( peekCString, peekCStringLen
    , newCString, newCStringLen
    , withCString, withCStringLen)
import qualified Foreign.C.String as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as B8
import System.Posix.Types
import System.Posix.IO hiding (openFd)
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.IO as S (openFd)
import qualified System.Posix.IO.ByteString as B (openFd)
import System.IO.Unsafe (unsafePerformIO)

class (Monoid s, IsString s) => FilePathLike s where
    asString :: s -> String
    peekCString :: CString -> IO s
    peekCStringLen :: CStringLen -> IO s
    withCString :: s -> (CString -> IO a) -> IO a 
    withCStringLen :: s -> (CStringLen -> IO a) -> IO a 
    unsafeWithCStringLen :: s -> (CStringLen -> IO a) -> IO a 
    openFd :: s -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO Fd
    dropTrailingSlash :: s -> s
    (</>) :: s -> s -> s
    splitFileName :: s -> (s, s)

instance FilePathLike [Char] where
    asString = id
    peekCString = S.peekCString
    peekCStringLen = S.peekCStringLen
    withCString = S.withCString
    withCStringLen = S.withCStringLen
    unsafeWithCStringLen = withCStringLen
    openFd = S.openFd
    dropTrailingSlash s = if null s' then "/" else s'
      where
        s' = dropWhileEnd (== '/') s
    _ </> s2@('/' : _) = s2
    s1 </> "" = s1
    "" </> s2 = s2
    s1 </> s2 = if s1' == "/" then '/' : s2 else s1' ++ "/" ++ s2
      where
        s1' = dropTrailingSlash s1
    splitFileName s = (if null d then "./" else reverse d, reverse n)
      where
        (n, d) = span (/= '/') $ reverse s

instance FilePathLike B.ByteString where
    asString = convert
    peekCString = B.packCString
    peekCStringLen = B.packCStringLen
    withCString = B.useAsCString
    withCStringLen = B.useAsCStringLen
    unsafeWithCStringLen = B.unsafeUseAsCStringLen
    openFd = B.openFd
    dropTrailingSlash s = if B.null s' then slashBS else s'
      where
        (s', _) = B8.spanEnd (== '/') s
    s1 </> s2
        | B.null s1 = s2
        | B.null s2 = s1
        | B8.head s2 == '/' = s2
        | otherwise =
            let s1' = dropTrailingSlash s1
            in  if B8.last s1' == '/' then slashBS <> s2
                                      else B.concat [s1', slashBS, s2]
    splitFileName s = (if B.null d then curDir else d, n)
      where
        (d, n) = B8.spanEnd (/= '/') s
        curDir = B8.pack "./"

convert :: (FilePathLike s1, FilePathLike s2) => s1 -> s2
convert s = unsafePerformIO $ unsafeWithCStringLen s peekCStringLen

slashBS :: B.ByteString
slashBS = B8.singleton '/'
