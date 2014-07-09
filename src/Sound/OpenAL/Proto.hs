-- | This module imports the entire package, except 'Sound.OpenAL.Proto.IO'.
module Sound.OpenAL.Proto (
-- * Overview
{-|
Audio playback and capture prototype with Haskell OpenAL bindings
 -}
-- * Sound.OpenAL.Proto
  module Sound.OpenAL.Proto.Capture,
  module Sound.OpenAL.Proto.Conversion,
  module Sound.OpenAL.Proto.Play,
  module Sound.OpenAL.Proto.Types,
  module Sound.OpenAL.Proto.UnitGen
) where

import Sound.OpenAL.Proto.Capture
import Sound.OpenAL.Proto.Conversion
import Sound.OpenAL.Proto.Play
import Sound.OpenAL.Proto.Types
import Sound.OpenAL.Proto.UnitGen
