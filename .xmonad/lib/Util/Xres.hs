-- A module for accessing Xresources.
module Util.Xres where

import Data.Bifunctor (bimap)
import Data.Char as DC
import Data.List as DL
import Data.Maybe (catMaybes, fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import XMonad.Core (installSignalHandlers)
import XMonad.Util.Run (runProcessWithInput)

splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> DL.elemIndex ':' str
  where
    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming s idx = bimap trim (trim . tail) $ splitAt idx s
    trim :: String -> String
    trim = DL.dropWhileEnd DC.isSpace . DL.dropWhile DC.isSpace

getFromXres :: String -> IO String
getFromXres key = do
  installSignalHandlers
  fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$>
      DL.find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

xProp :: String -> String
xProp = unsafePerformIO . getFromXres

-- Get font name in Xft format.
-- Example conversions:
-- JetBrainsMono Nerd Font:size=10:antialias=true -> JetBrainsMono Nerd Font
-- monospace:pixelsize=12 -> monospace
-- Font Name -> Font Name
xFont :: String
xFont =
  "xft:" ++ fst (fromMaybe (xProp "*.font", "") (splitAtColon (xProp "*.font")))

xFontSized :: String -> String
xFontSized s = xFont ++ ":size=" ++ s

xColorFg :: String
xColorFg = xProp "*.foreground"

xColorBg :: String
xColorBg = xProp "*.background"

xColor :: String -> String
xColor a = xProp $ "*.color" ++ a
