--
-- File     : ~/.xmonad/xmonad.hs
-- Author   : Thayer Williams
-- Website  : http://cinderwick.ca/
-- Desc     : A minimalist xmonad config I use for my netbook where I
--            spend most of my time working in Full mode.
--

import XMonad
import XMonad.Hooks.DynamicLog     -- statusbar 
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Layout.Named         -- custom layout names
import XMonad.Actions.DwmPromote   -- swap master like dwm
import XMonad.Util.Run(spawnPipe)  -- spawnPipe and hPutStrLn
import XMonad.Util.EZConfig        -- append key/mouse bindings
import System.IO                   -- hPutStrLn scope

import qualified XMonad.StackSet as W   -- for window manipulation (see W below)

main = do
        status <- spawnPipe myXmobar
        xmonad $ defaultConfig 
            { modMask            = mod4Mask
            , terminal           = "urxvtcd"
            , borderWidth        = 2
            , normalBorderColor  = "#dddddd"
            , focusedBorderColor = "#0000ff"
            , workspaces = myWorkspaces
            , layoutHook = myLayoutHook
            , manageHook = manageDocks <+> myManageHook
                            <+> manageHook defaultConfig
            , logHook    = myLogHook status
            } 
            `additionalKeysP` myKeys
            `additionalMouseBindings` myButtons

myLogHook h = dynamicLogWithPP $ myXmobarPP { ppOutput = hPutStrLn h }

myXmobar   = "xmobar ~/.xmonad/xmobarrc"
myXmobarPP = xmobarPP
    { ppCurrent = xmobarColor "#3399ff" "" . wrap " " " "
    , ppHidden  = xmobarColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = xmobarColor "#777777" "" . wrap " " " "
    , ppUrgent  = xmobarColor "#ff0000" "" . wrap " " " "
    , ppSep     = "     "
    , ppLayout  = xmobarColor "#aaaaaa" "" . wrap "·" "·"
    , ppTitle   = xmobarColor "#ffffff" "" . shorten 25
    }

-- default layout is fullscreen with smartborders
myLayoutHook = avoidStruts $ smartBorders ( full ||| mtiled ||| tiled )
  where
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled
    tiled   = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "Wicd-client.py" --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , className =? "Chromium"       --> doF (W.shift (myWorkspaces !! 1))
    , className =? "Nautilus"       --> doF (W.shift (myWorkspaces !! 2))
    , className =? "Gimp"           --> doF (W.shift (myWorkspaces !! 3))
    ]

myWorkspaces    = ["1","2","3","4","5"]

myKeys :: [(String, X())]
myKeys = [ ("M-b"        , sendMessage ToggleStruts              ) -- Toggle the status bar gap
         , ("M1-<Tab>"   , windows W.focusDown                   ) -- Move focus to the next window (alt-tab)
         , ("M-<Return>" , dwmpromote                            ) -- Swap the focused window and the master window
         , ("M-<Tab>"    , toggleWS                              ) -- Toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                                ) -- Go to next workspace
         , ("M-<Left>"   , prevWS                                ) -- Go to prev workspace
         , ("M-S-<Right>", shiftToNext                           ) -- Shift client to next workspace
         , ("M-S-<Left>" , shiftToPrev                           ) -- Shift client to prev workspace
         , ("M-c"        , spawn "xcalc"                         ) -- app launcher
         , ("M-p"        , spawn "gmrun"                         ) -- app launcher
         , ("M-n"        , spawn "wicd-client -n"                ) -- network manager
         , ("M-r"        , spawn "xmonad --restart"              ) -- Restart xmonad w/o recompiling
         , ("M-w"        , spawn "chromium"                      ) -- launch browser
         , ("M-S-w"      , spawn "chromium --incognito"          ) -- launch private browser
         , ("M-e"        , spawn "nautilus"                      ) -- launch file manager
         , ("C-M1-l"     , spawn "gnome-screensaver-command --lock"              ) -- lock screen
         , ("M-s"        , spawn "urxvtcd -e bash -c 'screen -dRR -S $HOSTNAME'" ) -- launch screen session
         , ("C-M1-<Delete>" , spawn "sudo shutdown -r now"       ) -- reboot
         , ("C-M1-<Insert>" , spawn "sudo shutdown -h now"       ) -- poweroff
         ]

myButtons = [ ((mod4Mask, button4), (\_ -> windows W.focusUp    ))
            , ((mod4Mask, button5), (\_ -> windows W.focusDown  )) 
            , ((0, 8), (\_ -> prevWS )) -- cycle workspaces 
            , ((0, 9), (\_ -> nextWS )) -- with thumb buttons 
            ]

-- vim:sw=4 sts=4 ts=4 tw=0 et ai 
