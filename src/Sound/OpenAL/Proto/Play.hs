module Sound.OpenAL.Proto.Play (
       memoryRegion,
       playSound,
       sineSound
) where

import Data.Word (Word8)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import GHC.Float
import Sound.ALUT
import Sound.ALUT.Loaders
import Sound.OpenAL.AL.BasicTypes (ALsizei)
import Sound.OpenAL.AL.Buffer
import Sound.OpenAL.Proto.Capture
import Sound.OpenAL.Proto.Conversion
import Sound.OpenAL.Proto.IO (
       checkAlErrors,
       checkAlcErrors,
       withFileContents
       )
import Sound.OpenAL.Proto.Types (sampleRate)
import Sound.OpenAL.Proto.UnitGen (sine)
import qualified Data.Vector.Storable as V

sineSound :: Integral a => Double -> a -> V.Vector Word8
sineSound freq secs = wavHeader $ take (sampleRate * fromIntegral(secs)) $ sine freq

memoryRegion :: V.Storable a => V.Vector a -> (MemoryRegion a)
memoryRegion v = MemoryRegion (unsafeForeignPtrToPtr fp) ((fromIntegral size)::ALsizei)
              where (fp, size) = V.unsafeToForeignPtr0 v

waitForSource :: Source -> IO ()
waitForSource s = do
    sstate <- get (sourceState s)
    case sstate of
      Playing ->
        do
          sleep 1
          waitForSource s
      _ -> return ()

playSound :: IO ()
playSound =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  let duration = (5.0::Float) in
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    -- buffer1 <- createBuffer HelloWorld
    -- buffer2 <- createBuffer $ Sine 220 0 0.5
    -- buffer1 <- createBuffer $ File "../../bjork-human-180secs-uint8.raw"
    -- buffer2 <- createBuffer $ File "../../../../Sounds/_Music samples/Bjork - Human Behaviour.wav"
    -- buffer2 <- withFileContents "../../../../Sounds/_Music samples/Bjork - Human Behaviour.wav" (createBuffer . FileImage)
    -- buffer2 <- createBuffer $ FileImage $ memoryRegion $ sineSound 220 2

    [source] <- genObjectNames 1

    -- [buffer1] <- genObjectNames 1
    [buffer2] <- genObjectNames 1

    capture Nothing (float2Double duration) $ \memRegion -> do
            bufferData buffer2 $= BufferData memRegion Mono16 (fromIntegral sampleRate)
            -- queueBuffers source [buffer1]
            queueBuffers source [buffer2]
            checkAlErrors
            checkAlcErrors device
            play [source]
            waitForSource source
    closeDevice device
    return ()

main = playSound
