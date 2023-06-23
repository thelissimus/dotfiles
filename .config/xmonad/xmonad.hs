import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

main :: IO ()
main = xmonad 
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp barCmd (pure barPP)) defToggleStrutsKey
     $ conf

conf = def
  { modMask     = mod4Mask
  , manageHook  = mHook
  , borderWidth = 1
  , terminal    = "sakura"
  }
  `additionalKeysP`
  [ ("M-S-z", spawn "xscreensaver-command -lock")
  , ("M-f"  , spawn "firefox")
  ]

barCmd = "xmobar ~/.config/xmobar/xmobarrc"

barPP :: PP
barPP = def
    { ppSep             = green " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . green . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, green, red, white, yellow :: String -> String
    green    = xmobarColor "#a9b665" ""
    blue     = xmobarColor "#7daea3" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

mHook :: ManageHook
mHook = composeAll
  [ isDialog --> doFloat
  ]
