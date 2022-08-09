import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myStartupHook = do
    spawn "$HOME/.xmonad/autostart.sh"
    setWMName "LG3D"
    setDefaultCursor xC_left_ptr

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

main = do
    xmproc <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobarrc"
    xmonad $ ewmh $ docks def
        { manageHook = myManageHook <+> manageHook def -- make sure to include myManageHook definition from above
        , layoutHook = avoidStruts  $  layoutHook def
        , startupHook = myStartupHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "urxvtc"
        , workspaces = ["web", "irc", "code" ] ++ map show [4..9]
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_p), spawn "rofi -show run")
        , ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
