import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Posix.Unistd
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Run

myTerminal :: String
myTerminal = "st"

myWorkspaces :: [PhysicalWorkspace]
myWorkspaces = withScreens 3 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

mySpacing ::
     Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayoutHook :: ModifiedLayout Spacing (ModifiedLayout AvoidStruts Tall) a
myLayoutHook = mySpacing 10 $ avoidStruts $ tiled
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

myNormalBorderColor :: String
myNormalBorderColor = "#282C34"

myFocusedBorderColor :: String
myFocusedBorderColor = "#E6E1CF"

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
    docks
      def
        { terminal = myTerminal
        , workspaces = myWorkspaces
        , modMask = mod4Mask
        , logHook =
            mapM_
              (\handle ->
                 dynamicLogWithPP $ xmobarPP {ppOutput = hPutStrLn handle})
              xmprocs >>
            updatePointer (0.5, 0.5) (0, 0)
        , keys = \c -> myKeys c `M.union` (keys def c)
        , layoutHook = myLayoutHook
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        }
