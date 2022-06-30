-- Xmonad is a dynamically tiling X11 window manager that is written and
-- configured in Haskell. Official documentation: https://xmonad.org

-- Base
import XMonad hiding ( (|||) )
import System.IO ( hPutStrLn )
import System.Exit
import qualified XMonad.StackSet as W

-- Data
import Control.Monad ( liftM2 )

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ( doFullFloat, doCenterFloat, isFullscreen, isDialog )
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

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
import XMonad.Util.EZConfig ( additionalKeysP, removeKeysP )
import XMonad.Util.Loggers
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

-- Workspaces
myWorkspaces :: [String]
myWorkspaces = ["1:web","2:irc","3:mail","4:dev","5:gfx","6:media","7:term","8:dir","9:tmp"]

-- Now run xmonad with all the defaults available
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc2" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask            = myModMask                                   -- Rebind Mod to the Super key
    , layoutHook         = smartBorders . avoidStruts $ myLayout       -- Use custom layouts
    , manageHook         = myManageHook <+> manageHook def             -- Match on certain windows
    , startupHook        = myStartupHook
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor  = myNormalBorderColor
    , borderWidth        = myBorderWidth
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    }

    `removeKeysP`

        [

        -- unused close window binding
        "M-S-c",

        -- unused terminal binding
        "M-S-<Return>",

        -- unused gmrun binding
        "M-S-p",

        -- unused exit Xmonad binding
        "M-S-q",

        -- unused dmenu binding
        "M-p",

        -- unused restart Xmonad binding
        "M-q"

        ]

        `additionalKeysP` myKeys

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

-- Window rules
myManageHook = composeAll . concat $
    [ [isDialog       --> doCenterFloat]
    , [className =? c --> doCenterFloat  | c <- myCFloats]
    , [title     =? t --> doFloat        | t <- myTFloats]
    , [resource  =? r --> doFloat        | r <- myRFloats]
    , [resource  =? i --> doIgnore       | i <- myIgnores]
    , [className =? c --> doShift (myWorkspaces !! 0) <+> viewShift (myWorkspaces !! 0) | c <- my1Shifts]
    , [className =? c --> doShift (myWorkspaces !! 1) <+> viewShift (myWorkspaces !! 1) | c <- my2Shifts]
    , [className =? c --> doShift (myWorkspaces !! 2) <+> viewShift (myWorkspaces !! 2) | c <- my3Shifts]
    , [className =? c --> doShift (myWorkspaces !! 3) <+> viewShift (myWorkspaces !! 3) | c <- my4Shifts]
    , [className =? c --> doShift (myWorkspaces !! 4) <+> viewShift (myWorkspaces !! 4) | c <- my5Shifts]
    , [className =? c --> doShift (myWorkspaces !! 5) <+> viewShift (myWorkspaces !! 5) | c <- my6Shifts]
    , [className =? c --> doShift (myWorkspaces !! 6) <+> viewShift (myWorkspaces !! 6) | c <- my7Shifts]
    , [className =? c --> doShift (myWorkspaces !! 7) <+> viewShift (myWorkspaces !! 7) | c <- my8Shifts]
    , [className =? c --> doShift (myWorkspaces !! 8) <+> viewShift (myWorkspaces !! 8) | c <- my9Shifts]
    ]
    where
    viewShift  = doF . liftM2 (.) W.greedyView W.shift
    myCFloats  = ["Galculator", "feh", "mpv", "Xfce4-terminal"]
    myTFloats  = ["Downloads", "Save As..."]
    myRFloats  = []
    myIgnores  = ["desktop_window"]
    my1Shifts  = ["Brave-browser", "Chromium", "Vivaldi-stable", "Firefox-esr"]
    my2Shifts  = ["Hexchat"]
    my3Shifts  = ["Mail", "Thunderbird"]
    my4Shifts  = ["Emacs", "Geany", "Sublime_text"]
    my5Shifts  = ["Gimp", "feh", "Inkscape", "Ristretto"]
    my6Shifts  = ["vlc", "mpv"]
    my7Shifts  = ["kitty", "st-256color", "Terminator", "URxvt"]
    my8Shifts  = ["Thunar", "nemo", "caja", "pcmanfm"]
    my9Shifts  = ["Transmission-gtk", "Uget-gtk"]

-- Key bindings
myKeys :: [(String, X ())]
myKeys = [ ("M-<Return>"   , spawn "st"                                       )
         , ("M-S-<Return>" , spawn "thunar"                                   )
         , ("M-S-t"        , spawn "urxvtc"                                   )
         , ("M-]"          , spawn "firefox-esr"                              )
         , ("M-S-="        , unGrab *> spawn "scrot -s"                       )
         , ("M-C-f"        , sendMessage $ JumpToLayout "Full"                )
         , ("M-C-l"        , spawn "slock"                                    )
         , ("M-C-q"        , io ( exitWith ExitSuccess )                      )
         , ("M-C-r"        , spawn $ "xmonad --recompile && xmonad --restart" )
         , ("M-w"          , kill                                             )
         , ("M1-d"         , spawn "dmenu_run"                                )
         , ("M1-e"         , spawn "emacsclient -c -a 'emacs'"                )
         , ("M1-f"         , spawn "thunar"                                   )
         , ("M1-g"         , spawn "geany"                                    )
         , ("M1-n"         , spawn "nitrogen"                                 )
         , ("M1-r"         , spawn "rofi -show run"                           )
         , ("M1-t"         , spawn "transmission-gtk"                         )
         , ("M1-u"         , spawn "uget-gtk"                                 )
         , ("M1-v"         , spawn "pavucontrol"                              )
         , ("M1-w"         , spawn "brave-browser"                            )
         , ("M1-C-m"       , spawn "/usr/local/src/thunderbird/thunderbird"   )
         , ("M1-C-s"       , spawn "/usr/local/src/sublime_text/sublime_text" )
         , ("M1-C-w"       , spawn "/usr/local/src/waterfox/waterfox-bin"     )
         ]

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

-- Status bar configurations
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
