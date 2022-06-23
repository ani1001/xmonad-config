-- base
import XMonad
import System.IO ( hPutStrLn )
import qualified XMonad.StackSet as W

-- actions
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatKeys
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.FlexibleResize as Flex

-- data
import Control.Monad ( liftM2 )
import Data.Monoid
import qualified Data.Map as M

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid

-- layouts modifiers
import XMonad.Layout.DragPane
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

-- utilities
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.SpawnOnce

-- others
import XMonad.Prompt
import XMonad.Prompt.Window
import Graphics.X11.ExtraTypes.XF86

-- local variables
myWorkspaces = ["1", "2", "3", "4", "5"]
modm = mod4Mask

-- original color setting
-- colorBlue     = "#868bae"
-- colorGreen    = "#00d700"
-- colorRed      = "#ff005f"
-- colorGray     = "#666666"
-- colorWhite    = "#bdbdbd"
-- colorNormalbg = "#1c1c1c"
-- colorfg       = "#a8b6b8"

-- soft color setting
-- colorBlue     = "#477ab3"
-- colorGreen    = "#52ad91"
-- colorRed      = "#52ad91"
-- colorGray     = "#4d4d4d"
-- colorWhite    = "#ffffff"
-- colorNormalbg = "#1b1b1b"
-- colorfg       = "#ffffff"

-- arc color setting
colorBlue     = "#d8dee9"
colorGreen    = "#5e81ac"
colorRed      = "#bf616a"
colorGray     = "#ededed"
colorWhite    = "#ffffff"
colorNormalbg = "#2e3440"
colorfg       = "#8fbcbb"


-- color of focused border
myFocusedBorderColor :: String
myFocusedBorderColor = "#cca8c9"

-- color of inactive border
myNormalBorderColor :: String
myNormalBorderColor = "#333333"

-- width of border around windows
borderwidth :: Dimension
borderwidth = 1

-- float window control width
moveWD = borderwidth
resizeWD = 2 * borderwidth

-- gapwidth
gapwidth  = 9
gwU = 1
gwD = 0
gwL = 42
gwR = 42

-- startup hook
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawnOnce "lxpolkit &"
  spawnOnce "urxvtd -q -o -f &"
  spawnOnce "emacs --daemon &"
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr

main :: IO ()
main = do
    wsbar <- spawnPipe myWsBar
    xmonad $ ewmh def
        { borderWidth        = borderwidth
        , terminal           = "st"
        , focusFollowsMouse  = True
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , manageHook         = myManageHookShift <+>
                               myManageHookFloat <+>
                               manageDocks
        , layoutHook         = avoidStruts $ ( toggleLayouts (noBorders Full)
                                           $ onWorkspace "3" simplestFloat
                                           $ myLayout
                                           
                                           )
        , startupHook = myStartupHook
       
        -- xmobar setting
        , logHook = myLogHook wsbar >> updatePointer (0.5,0.5) (0,0)
        , handleEventHook    = fullscreenEventHook <+> docksEventHook
        , workspaces         = myWorkspaces
        , modMask            = modm
        , mouseBindings      = newMouse
        }
       
        -- define keys to remove
        `removeKeysP`
       
        [
       
        -- unused gmrun binding
        "M-S-p",
       
        -- unused close window binding
        "M-S-c",
       
        "M-S-<Return>"
       
        ]
       
        -- keymap: window operations
        `additionalKeysP`
       
        [
       
        -- shrink / expand the focused window
          ("M-,"    , sendMessage Shrink)
        , ("M-."    , sendMessage Expand)
        , ("M-z"    , sendMessage MirrorShrink)
        , ("M-a"    , sendMessage MirrorExpand)
       
        -- close the focused window
        , ("M-S-c"    , kill1)
       
        -- toggle layout (Fullscreen mode)
        , ("M-f"    , sendMessage ToggleLayout)
        , ("M-S-f"  , withFocused (keysMoveWindow (-borderwidth,-borderwidth)))
       
        -- toggle layout (simplest float)
        , ("M-u"    , sendMessage (Toggle "Simplest"))
       
        -- move the focused window
        , ("M-C-<R>", withFocused (keysMoveWindow (moveWD, 0)))
        , ("M-C-<L>", withFocused (keysMoveWindow (-moveWD, 0)))
        , ("M-C-<U>", withFocused (keysMoveWindow (0, -moveWD)))
        , ("M-C-<D>", withFocused (keysMoveWindow (0, moveWD)))
       
        -- resize the focused window
        , ("M-s"    , withFocused (keysResizeWindow (-resizeWD, resizeWD) (0.5, 0.5)))
        , ("M-i"    , withFocused (keysResizeWindow (resizeWD, resizeWD) (0.5, 0.5)))
       
        -- increase / decrese the number of master pane
        , ("M-S-;"  , sendMessage $ IncMasterN 1)
        , ("M--"    , sendMessage $ IncMasterN (-1))
       
        -- go to the next / previous workspace
        , ("M-<R>"  , nextWS )
        , ("M-<L>"  , prevWS )
        , ("M-l"    , nextWS )
        , ("M-h"    , prevWS )
       
        -- shift the focused window to the next / previous workspace
        , ("M-S-<R>", shiftToNext)
        , ("M-S-<L>", shiftToPrev)
        , ("M-S-l"  , shiftToNext)
        , ("M-S-h"  , shiftToPrev)
       
        -- copy window
        , ("M-v"    , windows copyToAll)
        , ("M-S-v"  , killAllOtherCopies)
       
        -- move the focus down / up
        , ("M-<D>"  , windows W.focusDown)
        , ("M-<U>"  , windows W.focusUp)
        , ("M-j"    , windows W.focusDown)
        , ("M-k"    , windows W.focusUp)
       
        -- swap the focused window down / up
        , ("M-S-j"  , windows W.swapDown)
        , ("M-S-k"  , windows W.swapUp)
        , ("M-S-<D>"  , windows W.swapDown)
        , ("M-S-<U>"  , windows W.swapUp)
       
        -- shift the focused window to the master window
        , ("M-S-m"  , windows W.shiftMaster)
       
        -- search a window and focus into the window
        , ("M-g"    , windowPromptGoto myXPConfig)
       
        -- search a window and bring to the current workspace
        , ("M-b"    , windowPromptBring myXPConfig)
       
        -- move the focus to next screen (multi screen)
        , ("M-<Tab>", nextScreen)
       
        -- now we have more than one screen by dividing a single screen
        , ("M-C-<Space>", layoutScreens 2 (TwoPane 0.5 0.5))
        , ("M-C-S-<Space>", rescreen)
       
        ]

        -- keymap: moving workspace by number

        `additionalKeys`
       
        [ ((modm .|. m, k), windows $ f i)
            | (i, k) <- zip myWorkspaces
                [ xK_exclam, xK_at, xK_numbersign
                , xK_dollar, xK_percent, xK_asciicircum
                , xK_ampersand, xK_asterisk, xK_parenleft
                , xK_parenright
                ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ]

        -- keymap: custom commands
        
        `additionalKeysP`
       
        [
       
        -- zoomswap dwm like
          ("M-<Return>", dwmpromote) 
       
        -- launch audio player
       
        , ("M-w", spawn "audacious")
       
        -- launch ebook reader)
       
        -- , ("C-<Tab>", spawn "")
       
        -- launch terminal
       
        , ("M-S-<Return>", spawn "st")
       
        -- launch file manager
       
        , ("M-S-f", spawn "thunar")
       
        -- launch web browser
       
        , ("M-S-w", spawn "/usr/local/src/waterfox/waterfox-bin")
       
        -- launch dmenu for launching applicatiton
       
        , ("M-p", spawn "dmenu_run")
        , ("M-r", spawn "")
      
        -- toggle workspace
       
        , ("M-<Tab>", toggleWS)
       
        -- play / Pause media keys
       
        , ("<XF86AudioPlay>"  , spawn "ncmpcpp toggle")
        , ("<XF86HomePage>"   , spawn "ncmpcpp toggle")
        , ("S-<F6>"           , spawn "ncmpcpp toggle")
        -- , ("S-<XF86AudioPlay>", spawn "streamradio pause")
        -- , ("S-<XF86HomePage>" , spawn "streamradio pause")
       
        -- volume setting media keys
       
        --, ("<XF86AudioRaiseVolume>", spawn "sound_volume_change_wrapper.sh +")
        --, ("<XF86AudioLowerVolume>", spawn "sound_volume_change_wrapper.sh -")
        --, ("<XF86AudioMute>"       , spawn "sound_volume_change_wrapper.sh m")
       
        -- brightness Keys
        
        , ("<XF86MonBrightnessUp>"  , spawn "xbacklight + 5 -time 100 -steps 1")
        , ("<XF86MonBrightnessDown>", spawn "xbacklight - 5 -time 100 -steps 1")
       
        -- take a screenshot (whole window)
       
        , ("<Print>", spawn "scrot")
       
        ]

-- myLayout: handle window behaveior

-- myLayout = spacing gapwidth $ gaps [(U, gwU),(D, gwD),(L, gwL),(R, gwR)]
--           $ (ResizableTall 1 (1/204) (119/204) [])
--             ||| (TwoPane (1/204) (119/204))
--             ||| Simplest

myLayout =  tiled ||| mtiled ||| full ||| threecol ||| grid
    where
    nmaster  = 1     -- default number of windows in master pane
    delta    = 2/100 -- percentage of the screen to increment when resizing
    ratio    = 5/8   -- defaul proportion of the screen taken up by main pane
    rt       = spacing 5 $ ResizableTall nmaster delta ratio []
    tiled    = renamed [Replace "T"] $ smartBorders rt
    mtiled   = renamed [Replace "Bs"] $ smartBorders $ Mirror rt
    full     = renamed [Replace "M"] $ noBorders Full
    threecol = renamed [Replace "3c"] $ ThreeColMid 1 (3/100) (1/2)
    grid     = renamed [Replace "G"] $ GridRatio (3/3)

-- myManageHookShift: some window must created there

myManageHookShift = composeAll
            -- if you want to know className, type "$ xprop|grep CLASS" on shell
            [ className =? "Gimp"       --> mydoShift "3"
            ]
            where mydoShift = doF . liftM2 (.) W.greedyView W.shift

-- myManageHookFloat: new window will created in Float mode

myManageHookFloat = composeAll
    [ className =? "Gimp"             --> doFloat
    , className =? "SMPlayer"         --> doFloat
    , className =? "mpv"              --> doCenterFloat
    , className =? "feh"              --> doCenterFloat
    , className =? "Audacious"        --> doCenterFloat
    -- , className =? "Thunar"           --> doCenterFloat
    -- , className =? "Websearch"        --> doCenterFloat
    -- , title     =? "urxvtc_float"     --> doSideFloat SC
    , isFullscreen                    --> doFullFloat
    , isDialog                        --> doCenterFloat
    , stringProperty "WM_NAME" =? "LINE"                      --> (doRectFloat $ W.RationalRect 0.60 0.1 0.39 0.82)
    , stringProperty "WM_NAME" =? "Google Keep"               --> (doRectFloat $ W.RationalRect 0.3 0.1 0.4 0.82)
    , stringProperty "WM_NAME" =? "tmptex.pdf - 1/1 (96 dpi)" --> (doRectFloat $ W.RationalRect 0.29 0.25 0.42 0.5)
    , stringProperty "WM_NAME" =? "Figure 1"                  --> doCenterFloat
    ]

-- myLogHook: loghook settings

myLogHook h = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }

-- myWsBar: xmobar setting

myWsBar = "xmobar -x 0 $HOME/.config/xmobar/xmobarrc1"

wsPP = xmobarPP { ppOrder           = \(ws:l:t:_)  -> [ws,l,t]
                , ppCurrent         = xmobarColor colorRed     colorNormalbg . \s -> "●"
                , ppUrgent          = xmobarColor colorGray    colorNormalbg . \s -> "●"
                , ppVisible         = xmobarColor colorRed     colorNormalbg . \s -> "⦿"
                , ppHidden          = xmobarColor colorGray    colorNormalbg . \s -> "●"
                , ppHiddenNoWindows = xmobarColor colorGray    colorNormalbg . \s -> "○"
                , ppTitle           = xmobarColor colorRed     colorNormalbg
                , ppOutput          = putStrLn
                , ppWsSep           = " "
                , ppSep             = "  "
                }

-- myXPConfig: XPConfig

myXPConfig = def
                { font              = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
                , fgColor           = colorfg
                , bgColor           = colorNormalbg
                , borderColor       = colorNormalbg
                , height            = 35
                , promptBorderWidth = 0
                , autoComplete      = Just 100000
                , bgHLight          = colorNormalbg
                , fgHLight          = colorRed
                , position          = Bottom
                }
                
-- newMouse: right click is used for resizing window

myMouse x = [ ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
newMouse x = M.union (mouseBindings def x) (M.fromList (myMouse x))

-- vim: ft=haskell
