module Sound.OpenAL.Proto.UnitGen (
       sine
) where

import Sound.OpenAL.Proto.Types (Sample)

sine :: Double -> [Sample]
sine freq = cycle $ take n $ map sin [0, d..]
     where d = 2 * pi * freq / sr
           n = truncate (sr / freq)
           sr = 44100
