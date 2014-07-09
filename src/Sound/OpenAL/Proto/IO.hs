module Sound.OpenAL.Proto.IO (
       checkAlErrors,
       checkAlcErrors,
       withFileContents
) where

import Control.Exception (bracket)
import Control.Monad
import Foreign.Marshal.Alloc (allocaBytes)
import Sound.OpenAL.AL.Buffer
import System.IO (openBinaryFile, IOMode(ReadMode), hClose, hFileSize, hGetBuf)

-- import Control.Concurrent (threadDelay)
-- import Control.Exception (bracket, finally)
-- import Control.Monad (unless, when)
-- import Data.Int
import Data.List (intersperse)
-- import Data.Word
-- import Foreign
-- import Foreign.C.Types
-- import Foreign.ForeignPtr
-- import Foreign.ForeignPtr.Unsafe
-- import Foreign.Ptr
-- import Sound.ALUT
import Sound.OpenAL
-- import Sound.OpenAL.AL.BasicTypes (ALsizei)
import Sound.OpenAL.AL.Errors
-- import Sound.OpenAL.ALC.Capture
import Sound.OpenAL.ALC.Errors
-- import Sound.OpenAL.Proto.Conversion
-- import Sound.OpenAL.Proto.Types (sampleRate)
-- import Sound.OpenAL.Proto.UnitGen (sine)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
-- import qualified Data.Vector.Storable as V
-- import qualified Data.Vector.Storable.Mutable as VM

checkAlErrors :: IO ()
checkAlErrors = do
            errs <- get $ alErrors
            unless (null errs) $ do
                   hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
                   exitFailure

checkAlcErrors :: Device -> IO ()
checkAlcErrors device = do
            errs <- get $ alcErrors device
            unless (null errs) $ do
                   hPutStrLn stderr (concat (intersperse "," [ d | ALCError _ d <- errs ]))
                   exitFailure

--------------------------------------------------------------------------------

withFileContents :: FilePath -> (MemoryRegion a -> IO b) -> IO b
withFileContents filePath action =
   bracket (openBinaryFile filePath ReadMode) hClose $ \handle -> do
      numBytes <- fmap fromIntegral (hFileSize handle)
      allocaBytes numBytes $ \buf -> do
         bytesRead <- hGetBuf handle buf numBytes
         when (bytesRead /= numBytes) $
            ioError (userError "hGetBuf")
         action (MemoryRegion buf (fromIntegral numBytes))
