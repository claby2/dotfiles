module Main
  ( main
  ) where

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_MonBrightnessDown
  , xF86XK_MonBrightnessUp
  )
import Prompt.Bluetooth (bluetoothPrompt)
import System.Posix.Unistd (getSystemID, nodeName)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
  ( nextScreen
  , prevScreen
  , shiftNextScreen
  , shiftPrevScreen
  )
import XMonad.Actions.EasyMotion
  ( EasyMotionConfig(bgCol, cancelKey, emFont, overlayF)
  , bar
  , selectWindow
  )
import XMonad.Actions.Submap (submap)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, statusBarPipe)
import XMonad.Hooks.StatusBar.PP
  ( PP
  , ppCurrent
  , ppHidden
  , ppHiddenNoWindows
  , ppOrder
  , ppVisible
  , wrap
  , xmobarColor
  )
import XMonad.Layout.IndependentScreens
  ( PhysicalWorkspace
  , marshallPP
  , onCurrentScreen
  , withScreens
  , workspaces'
  )
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (WithBorder, noBorders)
import XMonad.Layout.Renamed (Rename(Replace), renamed)
import XMonad.Layout.Spacing (Border(Border), Spacing, spacingRaw)
import XMonad.Layout.ToggleLayouts
  ( ToggleLayout(Toggle)
  , ToggleLayouts
  , toggleLayouts
  )
import XMonad.Prompt
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh (sshPrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce (spawnOnce)

myTerminal :: String
myTerminal = "st"

myNormalBorderColor :: String
myNormalBorderColor = "#282C34"

myFocusedBorderColor :: String
myFocusedBorderColor = "#E6E1CF"

myWorkspaces :: [PhysicalWorkspace]
myWorkspaces = withScreens 3 $ fmap show [1 .. 9 :: Int]

barColor :: String
barColor = "#36A3D9"

barUnderline :: Maybe String -> String -> String
barUnderline Nothing = wrap "<box type=Bottom width=2 mb=2>" "</box>"
barUnderline (Just color) =
  wrap ("<box type=Bottom width=2 mb=2 color=" ++ color ++ ">") "</box>"

barPP :: ScreenId -> PP
barPP screen =
  marshallPP -- Turns a naive pretty-printer into one that is aware of the independent screens.
    screen
    def
      { ppOrder = \(wss:layout:_:_) -> [wss, layout]
      , ppHidden = barUnderline Nothing
      , ppHiddenNoWindows = id
      , ppCurrent = xmobarColor barColor "" . barUnderline (Just barColor)
      , ppVisible = xmobarColor barColor ""
      }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "startup"

windowSpacing :: Integer -> l a -> ModifiedLayout Spacing l a
windowSpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayoutHook ::
     ModifiedLayout AvoidStruts (ToggleLayouts (ModifiedLayout WithBorder Full) (ModifiedLayout Rename (ModifiedLayout Spacing Tall))) Window
myLayoutHook = avoidStruts $ toggleLayouts (noBorders Full) tiled
  where
    tiled =
      renamed [Replace "[]="] $ windowSpacing 10 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ manageDocks
    -- Float the window and makes it use the whole screen when a window requests to be fullscreen.
    , isFullscreen --> doFullFloat
    ]

promptConfig :: XPConfig
promptConfig =
  def
    { font = "xft:JetBrainsMono Nerd Font"
    , position = Top
    , promptBorderWidth = 0
    , height = 25
    , bgColor = "#0f1419"
    , fgColor = "#e6e1cf"
    , fgHLight = "#e6e1cf"
    , bgHLight = "#36a3d9"
    }

-- List of prompts and their associated key bindings.
promptList :: [(KeySym, XPConfig -> X ())]
promptList = [(xK_b, bluetoothPrompt), (xK_m, manPrompt), (xK_s, sshPrompt)]

easyMotionConfig :: EasyMotionConfig
easyMotionConfig =
  def
    { bgCol = "#0f1419"
    , cancelKey = xK_Escape
    , emFont = "xft:JetBrainsMono Nerd Font:pixelsize=30"
    , overlayF = bar (0.2 :: Double)
    }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
  M.fromList $
  -- Workspace binds for IndependentScreens.
  [ ((m .|. modm, k), windows $ onCurrentScreen f i)
  | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  -- Generate prompt binds from list.
  -- Uses submap to allow sub-mapping of key bindings:
  -- modm + xK_p + <PROMPT_BIND>
  [ ( (modm, xK_p)
    , submap . M.fromList $ [((modm, k), f promptConfig) | (k, f) <- promptList])
  ] ++
  -- Rest of the binds.
  [ ((modm, xK_Return), spawn myTerminal)
  , ((modm, xK_space), shellPrompt promptConfig)
  , ((modm, xK_w), kill1)
  , ((modm, xK_z), windows W.swapMaster)
  , ( (modm, xK_f)
    , selectWindow easyMotionConfig >>= (`whenJust` windows . W.focusWindow))
  , ( (modm, xK_q)
    , spawn
        "xmonad --recompile && xmonad --restart && notify-send \"xmonad Info\" \"Recompiled and restarted.\"")
  , ((modm .|. shiftMask, xK_f), sendMessage $ Toggle "Full")
  -- Multiple monitor handling.
  , ((modm, xK_minus), prevScreen)
  , ((modm, xK_equal), nextScreen)
  , ((modm .|. shiftMask, xK_minus), shiftPrevScreen >> prevScreen)
  , ((modm .|. shiftMask, xK_equal), shiftNextScreen >> nextScreen)
  -- Toggle spotify playback.
  , ( (modm .|. shiftMask, xK_p)
    , spawn "notify-send \"spotify-tui\" \"$(spt playback --toggle)\"")
  -- Screenshot.
  , ((modm .|. shiftMask, xK_s), spawn "flameshot gui")
  , ((modm .|. controlMask .|. shiftMask, xK_s), spawn "flameshot screen -c")
  -- Brightness control.
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
  ]

-- xmobar config file is dependent on host.
xmobarConfigPath :: String -> String
xmobarConfigPath hostname
  | hostname == "antique" = "~/.config/xmobar/xmobar.laptop.config"
  | otherwise = "~/.config/xmobar/xmobar.config"

barSpawner :: String -> ScreenId -> IO StatusBarConfig
barSpawner hostname s =
  statusBarPipe
    ("xmobar -x " ++ [last (show s)] ++ " " ++ xmobarConfigPath hostname) $
  pure (barPP s)

main :: IO ()
main = do
  hostname <- fmap nodeName getSystemID
  xmonad $
    ewmh $
    ewmhFullscreen $
    dynamicSBs (barSpawner hostname) $
    docks $
    def
      { terminal = myTerminal
      , normalBorderColor = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , workspaces = myWorkspaces
      , modMask = mod4Mask
      , logHook = updatePointer (0.5, 0.5) (0, 0) -- Automatic cursor warp.
      , startupHook = myStartupHook
      , layoutHook = myLayoutHook
      , manageHook = myManageHook <+> manageHook def
      , keys = \c -> myKeys c `M.union` keys def c
      }
