-- A prompt for connecting to bluetooth devices.
module Prompt.Bluetooth where

import Control.Exception as E
import XMonad (X, io)
import XMonad.Prompt
  ( XPConfig
  , XPrompt
  , mkComplFunFromList'
  , mkXPrompt
  , showXPrompt
  )
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

econst :: Monad m => a -> IOException -> m a
econst = const . return

data Bluetooth =
  Bluetooth

instance XPrompt Bluetooth where
  showXPrompt Bluetooth = "Connect: "

getBluetoothCompl :: IO [String]
getBluetoothCompl = do
  devices <-
    runProcessWithInput "sh" ["-c", "bluetoothctl devices | cut -d' ' -f2"] ""
  -- Return list of bluetooth devices.
  return $ lines devices

bluetoothPrompt :: XPConfig -> X ()
bluetoothPrompt c = do
  devices <- io getBluetoothCompl -- Lift IO.
  mkXPrompt
    Bluetooth
    c
    (mkComplFunFromList' devices)
    (\x -> safeSpawn "bluetoothctl" ["connect", x])
