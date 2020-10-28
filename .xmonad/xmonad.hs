{-# LANGUAGE TupleSections #-}

-- Base
import Control.Arrow                      ( (***) )
import Control.Monad                      ( void )
import Data.Bits                          ( (.|.) )
import Data.Char                          ( toUpper )
import Data.List                          ( uncons )
import Text.Printf                        ( printf )

-- Third-party
import Data.Default                       ( def )
import Graphics.X11.ExtraTypes.XF86       ( xF86XK_AudioLowerVolume
                                          , xF86XK_AudioMicMute
                                          , xF86XK_AudioMute
                                          , xF86XK_AudioNext
                                          , xF86XK_AudioPlay
                                          , xF86XK_AudioPrev
                                          , xF86XK_AudioRaiseVolume
                                          , xF86XK_Display
                                          , xF86XK_MonBrightnessDown
                                          , xF86XK_MonBrightnessUp
                                          , xF86XK_ScreenSaver )
import Graphics.X11.Types                 ( mod4Mask, noModMask, shiftMask
                                          , xK_Print, xK_a, xK_d, xK_f, xK_o
                                          , xK_s )
import System.Taffybar.Support.PagerHints ( pagerHints )
import XMonad.Core                        ( XConfig(..) )
import XMonad.Hooks.EwmhDesktops          ( ewmh )
import XMonad.Hooks.ManageDocks           ( avoidStruts, docks )
import XMonad.Main                        ( xmonad )
import XMonad.Util.EZConfig               ( additionalKeys )
import XMonad.Util.Run                    ( safeSpawn, safeSpawnProg )

pactl :: String -> Int -> [String]
pactl s n = case n of 0 -> cmd "mute"   $ "toggle"
                      _ -> cmd "volume" $ printf "%+d%%" n
  where def     = printf "@DEFAULT_%s@" $ map toUpper s
        cmd k v = ["pactl", printf "set-%s-%s" s k, def, v]

main :: IO ()
main = xmonad . docks . ewmh . pagerHints $ additionalKeys def
     { borderWidth        = 2
     , focusedBorderColor = "Green"
     , focusFollowsMouse  = False
     , layoutHook         = avoidStruts $ layoutHook def
     , modMask            = modMask'
     , normalBorderColor  = "Dim Gray"
     , terminal           = "x-terminal-emulator"
     } $

     mapPairs ((noModMask,), safeSpawn')
              [ (xK_Print,                ["scrot"])
              , (xF86XK_AudioLowerVolume, pactl "sink"   (-5))
              , (xF86XK_AudioMicMute,     pactl "source"    0)
              , (xF86XK_AudioMute,        pactl "sink"      0)
              , (xF86XK_AudioRaiseVolume, pactl "sink"      5)
              , (xF86XK_Display,          ["arandr"])
              , (xF86XK_ScreenSaver,      ["blc-lock"])
              ]

     ++
     mapPairs ((noModMask,), safeSpawn "playerctl")
              [ (xF86XK_AudioNext, ["next"      ])
              , (xF86XK_AudioPlay, ["play-pause"])
              , (xF86XK_AudioPrev, ["previous"  ])
              ]

     ++
     mapPairs ((noModMask,), safeSpawn "xbacklight")
              [ (xF86XK_MonBrightnessDown, ["-dec", lightStep])
              , (xF86XK_MonBrightnessUp,   ["-inc", lightStep])
              ]

     ++
     mapPairs ((modMask',), safeSpawn')
              [ (xK_a, ["sensible-editor" ])
              , (xK_d, ["signal-desktop"  ])
              , (xK_f, ["nautilus"        ])
              , (xK_o, ["passmenu"        ])
              , (xK_s, ["sensible-browser"])
              ]

     ++
     mapPairs ((modMask' .|. shiftMask,), safeSpawn')
              [ (xK_d, ["discord"])
              , (xK_s, ["sensible-browser", "-private-window", "--incognito"])
              ]

  where
    volStep    = 5.0
    lightStep  = "10"
    modMask'   = mod4Mask
    mapPairs   = map . uncurry (***)
    safeSpawn' = maybe mempty (uncurry safeSpawn) . uncons
