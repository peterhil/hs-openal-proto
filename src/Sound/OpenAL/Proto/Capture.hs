module Sound.OpenAL.Proto.Capture (
       bufferSize,
       capture,
       withCaptureDevice
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (unless, when)
import Data.Int
import Data.List (intersperse)
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Sound.ALUT
import Sound.OpenAL
import Sound.OpenAL.AL.BasicTypes (ALsizei)
import Sound.OpenAL.ALC.Capture
import Sound.OpenAL.Proto.Conversion
import Sound.OpenAL.Proto.Types (sampleRate)
import Sound.OpenAL.Proto.UnitGen (sine)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

--------------------------------------------------------------------------------

bufferSize :: Storable a => Int -> a -> Double -> Int
bufferSize nchannels sampleType secs = fromIntegral (numSamples secs) * sizeOf sampleType * nchannels

mono8BufferSize = bufferSize 1 (undefined::Word8)
mono16BufferSize = bufferSize 1 (undefined::Int16)
stereo8BufferSize = bufferSize 2 (undefined::Word8)
stereo16BufferSize = bufferSize 2 (undefined::Int16)

numSamples :: Double -> NumSamples
numSamples secs = round (fromIntegral(sampleRate) * secs) :: NumSamples

--------------------------------------------------------------------------------

-- # From http://dev.stephendiehl.com/hask/#ffi

vecPtr :: VM.MVector s CInt -> ForeignPtr CInt
vecPtr = fst . VM.unsafeToForeignPtr0

--------------------------------------------------------------------------------

-- captureSamples :: Device -> Ptr a -> NumSamples -> IO ()
-- allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
-- allocaBytesAligned :: Int -> Int -> (Ptr a -> IO b) -> IO b

withCaptureDevice :: (Maybe String) -> Double -> (Device -> IO c) -> IO c
withCaptureDevice specifier secs action =
    bracket
        (let format = Mono16 in -- TODO handle format more flexibly
         do (Just mic) <- captureOpenDevice specifier (fromIntegral sampleRate) format (numSamples secs)
            captureStart mic
            return mic
            )
        (\mic -> do captureStop mic
                    captureCloseDevice mic
                    )
        (action)

-- capture :: V.Storable a => (Maybe String) -> Double -> IO (MemoryRegion a)
-- capture :: Storable a => (Maybe String) -> Double -> (MemoryRegion a -> IO c) -> IO c
-- capture :: (Maybe String) -> Double -> IO (V.Vector Int16)
capture specifier duration action =
        let num = (numSamples duration)
            bytes = (mono16BufferSize duration)
        in
        do
            withCaptureDevice specifier duration $ \mic -> do
                sleep 3

                -- mutableV <- V.thaw $ V.replicate (fromIntegral num) (fromIntegral 0)
                mutableV <- V.thaw $ V.fromList $ map (pcm 16) $ take (fromIntegral num) $ sine 220
                withForeignPtr (vecPtr mutableV) $ \ptr -> do
                    captureSamples mic ptr (fromIntegral num)
                rec <- V.freeze mutableV

                -- recordedBytes <- get $ captureNumSamples mic
                -- putStrLn $ "Got samples: " ++ (show recordedBytes)

                let (mem, size) = V.unsafeToForeignPtr0 rec
                withForeignPtr mem $ \ptr -> do
                    action (
                       MemoryRegion
                       ptr
                       (fromIntegral size) -- (fromIntegral recordedBytes)
                       )
