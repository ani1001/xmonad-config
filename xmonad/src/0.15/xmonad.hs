-- Xmonad is a dynamically tiling X11 window manager that is written and
-- configured in Haskell. Official documentation: https://xmonad.org

-- Base
import XMonad hiding ( (|||) )
import System.IO ( hPutStrLn )
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

-- Layouts modifiers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

-- Utilities
import XMonad.Util.Cursor
import XMonad.Util.EZConfig ( additionalKeysP )
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab

-- Whether focus follows the mouse pointer
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- "windows key" is usually mod4Mask
myModMask :: KeyMask
myModMask = mod4Mask

-- Sets default terminal
myTerminal :: String
myTerminal = "st"

-- Color of focused border
myFocusedBorderColor :: String
myFocusedBorderColor = "#5e81ac"

-- Color of inactive border
myNormalBorderColor :: String
myNormalBorderColor = "#a3be8c"

-- Width of border around windows
myBorderWidth :: Dimension
myBorderWidth = 1

-- Sets default font
myFont :: String
myFont = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"

-- Counts the number of window
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Workspaces
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
             $ ["1:web","2:irc","3:mail","4:dev","5:comm","6:tmp","7:dvi","8","9"]
  where
    clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                  (i,ws) <- zip [1..9] l,
                  let n = i ]

-- Startup hook
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom -b &"
    spawnOnce "lxpolkit &"
    spawnOnce "mpd &"
    spawnOnce "urxvtd -q -o -f &"
    spawnOnce "emacs --daemon &"
    setWMName "LG3D"
    setDefaultCursor xC_left_ptr

-- The available layouts
myLayout =
  tiled
  ||| Mirror tiled
  ||| Full
  ||| emptyBSP
  ||| Grid
  ||| spiral ( 6 / 7 )
  ||| twopane
  ||| Mirror twopane
  ||| threeCol
  ||| noBorders (tabbed shrinkText def)
  ||| Accordion

  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    twopane  = TwoPane delta ratio
    tiled    = ResizableTall nmaster delta ratio []
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

-- Window rules
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , isDialog                      --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Xmessage"       --> doFloat
    , className =? "Firefox"        --> doShift "1:web"
    , className =? "Rhythmbox"      --> doShift "8"
    , className =? "XDvi"           --> doShift "7:dvi"
    ]

-- Status bars and logging
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 1.0

-- Key bindings
myKeys :: [(String, X ())]
myKeys = [ ("M-S-z"    , spawn "slock"                     )
         , ("M-S-="    , unGrab *> spawn "scrot -s"        )
         , ("M-]"      , spawn "firefox"                   )
         , ("M-S-p"    , spawn "rofi -show run"            )
         , ("M-S-t"    , spawn "urxvtc"                    )
         , ("M-C-f"    , sendMessage $ JumpToLayout "Full" )
         ]

-- Now run xmonad with all the defaults available
main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc1"
    xmonad . ewmh . docks $ def
        { manageHook = myManageHook <+> manageHook def
        , layoutHook = smartBorders . avoidStruts $ myLayout
        , startupHook = myStartupHook
        , logHook = myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc0
                        , ppTitle = xmobarColor "green" "" . shorten 35           -- Title of active window
                        , ppSep =  "<fc=" ++ "magenta" ++ "> <fn=1>|</fn> </fc>"  -- Separator character
                        , ppExtras  = [windowCount]                               -- Adding # of windows on current workspace to the bar
                        , ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t]             -- order of things in xmobar
                        }
        , modMask = myModMask
        , focusedBorderColor = myFocusedBorderColor
        , normalBorderColor = myNormalBorderColor
        , borderWidth = myBorderWidth
        , terminal = myTerminal
        , workspaces = myWorkspaces
        } `additionalKeysP` myKeys
