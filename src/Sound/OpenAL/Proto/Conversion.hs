{-# LANGUAGE FlexibleContexts #-}

module Sound.OpenAL.Proto.Conversion (
       asArray,
       audioSamples,
       pcm,
       pcm16,
       pcm8,
       sample,
       sample8,
       wav,
       wavHeader
) where

import Codec.ByteString.Builder
import Codec.Wav
import Data.Array.IO
import Data.Array.Unboxed
import Data.Int
import Data.Vector.Storable.ByteString
import Data.Word
import Sound.OpenAL.Proto.Types (Sample)
-- import qualified Data.Array.Repa as R
-- import qualified Data.Array.Repa.Repr.Unboxed as UV
import qualified Data.Audio as Au
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V

-- # PCM data

pcm :: (Integral a) => Int -> Sample -> a
pcm bits sample = truncate $ sample * (fromIntegral (2 ^ (bits - 1)) - 1)

pcm16 :: Sample -> Int16
pcm16 x = fromIntegral (pcm 16 x)

pcm8 :: Sample -> Word8
pcm8 x = fromIntegral (pcm 8 x) + 128

sample = asArray . map pcm16
sample8 = asArray . map pcm8

-- # Array conversion

-- asArray :: UV.Unbox a => [a] -> R.Array R.U (R.Z R.:. Int) a
-- asArray l = R.fromListUnboxed (R.Z R.:. ((length l) :: Int)) l

asArray l = V.fromList l

wavHeader :: V.Storable a => [Sample] -> V.Vector a
wavHeader = (wav 44100 1) . (audioSamples . map pcm16)

wav :: (IArray UArray a, Au.Audible a, AudibleInWav a, V.Storable b) => Int -> Int -> Au.SampleData a -> V.Vector b
wav rate nchannels audio = convert $ buildWav $ Au.Audio rate nchannels audio
    where convert = byteStringToVector . L.toStrict . toLazyByteString

audioSamples :: (IArray UArray a) => [a] -> Au.SampleData a
audioSamples l = listArray (0, (length l)) l
