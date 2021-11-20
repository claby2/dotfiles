-- A prompt for connecting to bluetooth devices.
module Prompt.Bluetooth where

import XMonad (X, io)
import XMonad.Prompt (XPConfig, mkComplFunFromList')
import XMonad.Prompt.Input ((?+), inputPromptWithCompl)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

bluetoothPrompt :: XPConfig -> X ()
bluetoothPrompt c = do
  devices <- io getBluetoothCompl -- Lift IO.
  inputPromptWithCompl c "Bluetooth device" (mkComplFunFromList' c devices) ?+
    connectDevice
  where
    getBluetoothCompl :: IO [String]
    getBluetoothCompl = do
      devices <-
        runProcessWithInput
          "sh"
          ["-c", "bluetoothctl devices | cut -d' ' -f2"]
          ""
      return $ lines devices -- Return list of bluetooth devices.
    connectDevice :: String -> X ()
    connectDevice dev = safeSpawn "bluetoothctl" ["connect", dev]
