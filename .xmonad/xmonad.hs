import XMonad
import XMonad.Hooks.DynamicLog
import Data.Monoid
import System.Exit

-- Terminal emulator
myTerminal = "roxterm"

-- Whether focus follows mouse pointer
myFocusFollowsMouse = True

-- Width of window border (px)
myBorderWidth = 1

-- Mod key - Super
myModMask = mod4Mask

-- Border colours
myNormalBorderColour  = "#DDDDDD"
myFocusedBorderColour = "#87CF3E"

-- Float Gimp
myManageHook = composeAll [className =? "Gimp" --> doFloat]

main = xmonad =<< xmobar defaultConfig
     { terminal           = myTerminal
     , focusFollowsMouse  = myFocusFollowsMouse
     , borderWidth        = myBorderWidth
     , modMask            = myModMask
     , normalBorderColor  = myNormalBorderColour
     , focusedBorderColor = myFocusedBorderColour
     , manageHook         = myManageHook
     }
