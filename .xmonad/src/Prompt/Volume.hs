-- A prompt for setting output volume through amixer.
module Prompt.Volume where

import XMonad (X)
import XMonad.Prompt (XPConfig)
import XMonad.Prompt.Input ((?+), inputPrompt)
import XMonad.Util.Run (unsafeSpawn)

volumePrompt :: XPConfig -> X ()
volumePrompt c = inputPrompt c "Volume %" ?+ setVolume
  where
    setVolume :: String -> X ()
    setVolume vol = unsafeSpawn $ "amixer set Master \"" ++ vol ++ "%\""
