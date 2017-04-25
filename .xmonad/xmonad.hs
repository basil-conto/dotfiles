
-- Base
import Data.Maybe                   ( fromMaybe )
import Text.Printf                  ( printf )

-- Third-party
import Data.Default                 ( def )
import Graphics.X11.Types           ( mod4Mask, noModMask
                                    -- , xK_Print
                                    )
import Graphics.X11.ExtraTypes.XF86 ( xF86XK_AudioLowerVolume
                                    , xF86XK_AudioMicMute
                                    , xF86XK_AudioMute
                                    -- , xF86XK_AudioNext
                                    -- , xF86XK_AudioPlay
                                    -- , xF86XK_AudioPrev
                                    , xF86XK_AudioRaiseVolume
                                    -- , xF86XK_Display
                                    -- , xF86XK_Launch1
                                    -- , xF86XK_MonBrightnessDown
                                    -- , xF86XK_MonBrightnessUp
                                    -- , xF86XK_ScreenSaver
                                    -- , xF86XK_Sleep
                                    -- , xF86XK_WebCam
                                    -- , xF86XK_WLAN
                                    )
import XMonad.Core                  ( X(), XConfig(..) )
import XMonad.Hooks.ManageDocks     ( avoidStruts, manageDocks )
import XMonad.Main                  ( xmonad )
import XMonad.ManageHook            ( (<+>) )
import XMonad.Util.EZConfig         ( additionalKeys )
import XMonad.Util.Run              ( runProcessWithInput, safeSpawn )

zenburnAlist :: [(String, String)]
zenburnAlist = [ ("bg-1", "#2B2B2B")
               , ("bg"  , "#3F3F3F")
               , ("blue", "#8CD0D3")
               ]

speaker :: Bool -> Int -> String
speaker m v | m         = "🔇"
            | v ==  0   = "🔈"
            | v  < 60   = "🔉"
            | otherwise = "🔊"

speaker' :: Bool -> Int -> String
speaker' m v | m         = "mute"
             | v ==  0   = "zero"
             | v  < 60   = "low"
             | otherwise = "high"

-- FIXME:
-- * Change mute, low, etc. message according to button, not status
-- * Configure Unicode font with speaker instead of speaker'
pactl :: [String] -> X ()
pactl args = do
  [m, v] <- words <$> pamixer (["--get-mute", "--get-volume"] ++ args)
  safeSpawn "osd_cat"
    $ (fmt m v) ++ [ "--align=center"
                   , "--barmode=percentage"
                   , "--color=#8CD0D3"
                   , "--delay=1"
                   , "--font=-*-*-*-*-*-*-20-*-*-*-*-*-*-*"
                   , "--outline=1"
                   , "--pos=bottom"
                   ]
  return ()

  where
    pamixer as = runProcessWithInput "pamixer" as ""
    fmt m v    = [ "--percentage=" ++ v
                 , printf "--text=%3s%% %s" v $ speaker' (m == "true") (read v)
                 ]

main :: IO ()
main = xmonad $ def
     { borderWidth        = 1
     , focusedBorderColor = zenburn "blue" focusedBorderColor
     , focusFollowsMouse  = False
     , layoutHook         = avoidStruts  $  layoutHook def
     , manageHook         = manageDocks <+> manageHook def
     , modMask            = mod4Mask
     , normalBorderColor  = zenburn "bg-1" normalBorderColor
     , terminal           = "x-terminal-emulator"
     }

     `additionalKeys`
     [ ((noModMask, xF86XK_AudioLowerVolume), pactl [ "--decrease", step ])
     , ((noModMask, xF86XK_AudioRaiseVolume), pactl [ "--increase", step ])
     , ((noModMask, xF86XK_AudioMute       ), pactl [ "--toggle-mute"    ])
     , ((noModMask, xF86XK_AudioMicMute    ), pactl [ "--toggle-mute"
                                                    , "--default-source"
                                                    ])
     ]

  where
    step        = "2"
    zenburn k f = fromMaybe (f def) (lookup k zenburnAlist)
