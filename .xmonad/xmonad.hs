import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Posix.Unistd
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Run

myTerminal :: String
myTerminal = "st"

myNormalBorderColor :: String
myNormalBorderColor = "#282C34"

myFocusedBorderColor :: String
myFocusedBorderColor = "#E6E1CF"

myWorkspaces :: [PhysicalWorkspace]
myWorkspaces = withScreens 3 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

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
myStartupHook = setFullscreenSupport

windowSpacing :: Integer -> l a -> ModifiedLayout Spacing l a
windowSpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayoutHook ::
     ModifiedLayout Spacing (ModifiedLayout AvoidStruts (ToggleLayouts (ModifiedLayout WithBorder Full) Tall)) Window
myLayoutHook =
  windowSpacing 10 $ avoidStruts $ toggleLayouts (noBorders Full) tiled
  where
    tiled = Tall nmaster delta ratio
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

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
  M.fromList $
  -- Workspace binds for IndependentScreens.
  [ ((m .|. modm, k), windows $ onCurrentScreen f i)
  | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [ ((modm, xK_Return), spawn myTerminal)
  , ( (modm, xK_space)
    , shellPrompt
        def
          { font = "xft:JetBrainsMono Nerd Font"
          , position = Top
          , promptBorderWidth = 0
          , height = 25
          })
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
      [0 .. nScreens - 1 :: Integer]
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
            mapM_
              (\handle ->
                 dynamicLogWithPP $ xmobarPP {ppOutput = hPutStrLn handle})
              xmprocs >>
            updatePointer (0.5, 0.5) (0, 0)
        , startupHook = myStartupHook
        , layoutHook = myLayoutHook
        , manageHook = myManageHook <+> manageHook def
        , handleEventHook = docksEventHook <+> fullscreenEventHook
        , keys = \c -> myKeys c `M.union` (keys def c)
        }
