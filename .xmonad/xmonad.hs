import qualified Data.Map as M
import GHC.IO.Handle.Types (Handle)
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
  , prevScreen
  , shiftNextScreen
  , shiftPrevScreen
  )
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
  ( PP
  , dynamicLogWithPP
  , ppCurrent
  , ppHidden
  , ppHiddenNoWindows
  , ppOrder
  , ppOutput
  , ppVisible
  , wrap
  , xmobarColor
  )
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
  ( AvoidStruts
  , avoidStruts
  , docks
  , docksEventHook
  , manageDocks
  )
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, transience')
import XMonad.Layout.IndependentScreens
  ( PhysicalWorkspace
  , countScreens
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
import XMonad.Prompt.Shell (shellPrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myTerminal :: String
myTerminal = "st"

myNormalBorderColor :: String
myNormalBorderColor = "#282C34"

myFocusedBorderColor :: String
myFocusedBorderColor = "#E6E1CF"

myWorkspaces :: [PhysicalWorkspace]
myWorkspaces = withScreens 3 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

barColor :: String
barColor = "#36A3D9"

barUnderline :: Maybe String -> String -> String
barUnderline Nothing = wrap "<box type=Bottom width=2 mb=2>" "</box>"
barUnderline (Just color) =
  wrap ("<box type=Bottom width=2 mb=2 color=" ++ color ++ ">") "</box>"

barPP :: Handle -> ScreenId -> PP
barPP handle screen =
  marshallPP -- Turns a naive pretty-printer into one that is aware of the independent screens.
    screen
    def
      { ppOrder = \(wss:layout:_:_) -> [wss, layout]
      , ppHidden = barUnderline Nothing
      , ppHiddenNoWindows = id
      , ppCurrent = xmobarColor barColor "" . barUnderline (Just barColor)
      , ppVisible = xmobarColor barColor ""
      , ppOutput = hPutStrLn handle
      }

setFullscreenSupport :: X ()
setFullscreenSupport =
  withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <-
      mapM
        getAtom
        [ "_NET_WM_STATE_HIDDEN"
        , "_NET_WM_STATE_FULLSCREEN"
        , "_NET_NUMBER_OF_DESKTOPS"
        , "_NET_CLIENT_LIST"
        , "_NET_CLIENT_LIST_STACKING"
        , "_NET_CURRENT_DESKTOP"
        , "_NET_DESKTOP_NAMES"
        , "_NET_ACTIVE_WINDOW"
        , "_NET_WM_DESKTOP"
        , "_NET_WM_STRUT"
        ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

myStartupHook :: X ()
myStartupHook = do
  setFullscreenSupport
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
    -- Check to see if a window is transient, and then move it to its parent.
    , transience'
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
    }

-- List of prompts and their associated key bindings.
promptList :: [(KeySym, XPConfig -> X ())]
promptList = [(xK_space, shellPrompt), (xK_b, bluetoothPrompt)]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
  M.fromList $
  -- Workspace binds for IndependentScreens.
  [ ((m .|. modm, k), windows $ onCurrentScreen f i)
  | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  -- Generate prompt binds from list.
  [((modm, k), f promptConfig) | (k, f) <- promptList] ++
  -- Rest of the binds.
  [ ((modm, xK_Return), spawn myTerminal)
  , ((modm, xK_w), kill1)
  , ((modm, xK_z), windows W.swapMaster)
  , ( (modm, xK_q)
    , spawn
        "xmonad --recompile && xmonad --restart && notify-send \"xmonad Info\" \"Recompiled and restarted.\"")
  , ((modm .|. shiftMask, xK_f), sendMessage $ Toggle "Full")
  -- Multiple monitor handling.
  , ((modm, xK_o), nextScreen)
  , ((modm .|. shiftMask, xK_o), shiftNextScreen >> nextScreen)
  , ((modm .|. controlMask, xK_o), prevScreen)
  , ((modm .|. shiftMask .|. controlMask, xK_o), shiftPrevScreen >> prevScreen)
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

main :: IO ()
main = do
  nScreens <- countScreens
  hostname <- fmap nodeName getSystemID
  -- Dynamically spawn xmobar to each screen.
  xmprocs <-
    mapM
      (\i ->
         spawnPipe $ "xmobar -x " ++ show i ++ " " ++ xmobarConfigPath hostname)
      [0 .. nScreens - 1 :: Int]
  xmonad $
    ewmh $
    docks
      def
        { terminal = myTerminal
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        , modMask = mod4Mask
        , logHook =
            mapM_ dynamicLogWithPP (zipWith barPP xmprocs [0 .. (S nScreens)]) >>
            updatePointer (0.5, 0.5) (0, 0)
        , startupHook = myStartupHook
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook def
        , handleEventHook = docksEventHook <+> fullscreenEventHook
        , keys = \c -> myKeys c `M.union` (keys def c)
        }
