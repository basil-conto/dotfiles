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
                                          , xF86XK_Favorites
                                          , xF86XK_Go
                                          , xF86XK_MonBrightnessDown
                                          , xF86XK_MonBrightnessUp
                                          , xF86XK_ScreenSaver )
import Graphics.X11.Types                 ( mod4Mask, noModMask, shiftMask
                                          , xK_Cancel, xK_Print, xK_a, xK_b
                                          , xK_d, xK_f, xK_g, xK_l, xK_o, xK_s
                                          , xK_t, xK_u, xK_v, xK_x, xK_y, xK_z )
import System.Directory                   ( getHomeDirectory )
import XMonad.Core                        ( XConfig(..) )
import XMonad.Hooks.EwmhDesktops          ( ewmh )
import XMonad.Hooks.ManageDocks           ( ToggleStruts(..)
                                          , avoidStruts, docks )
import XMonad.Hooks.TaffybarPagerHints    ( pagerHints )
import XMonad.Layout.NoBorders            ( smartBorders )
import XMonad.Main                        ( xmonad )
import XMonad.Operations                  ( sendMessage )
import XMonad.Util.EZConfig               ( additionalKeys )
import XMonad.Util.Hacks                  ( javaHack )
import XMonad.Util.Run                    ( safeSpawn, safeSpawnProg )

wpctl :: String -> Int -> [String]
wpctl s n
  | n == 0    = cmd "mute" "toggle"
  | n <  0    = vol '-'
  | otherwise = vol '+'
  where def     = printf "@DEFAULT_AUDIO_%s@" $ map toUpper s
        cmd k v = ["wpctl", printf "set-%s" k, def, v]
        vol sgn = cmd "volume" $ printf "%d%%%c" (abs n) sgn

main :: IO ()
main = do
  home <- getHomeDirectory
  let volStep    = 5
      modMask'   = mod4Mask
      mapPairs   = map . uncurry (***)
      safeSpawn' = maybe mempty (uncurry safeSpawn) . uncons

  xmonad . docks . javaHack . ewmh . pagerHints $ additionalKeys def
    { borderWidth        = 2
    , focusedBorderColor = "#8adf80" -- modus-operandi bg-green-intense
    , focusFollowsMouse  = False
    , layoutHook         = smartBorders . avoidStruts $ layoutHook def
    , modMask            = modMask'
    , normalBorderColor  = "#e0f6e0" -- modus-operandi bg-green-nuanced
    , terminal           = "x-terminal-emulator"
    } $

    mapPairs ((noModMask,), safeSpawn')
             [ (xK_Print,                ["flameshot", "full", "--path", home])
             , (xF86XK_AudioLowerVolume, wpctl "sink"   (-volStep))
             , (xF86XK_AudioMicMute,     wpctl "source"         0 )
             , (xF86XK_AudioMute,        wpctl "sink"           0 )
             , (xF86XK_AudioRaiseVolume, wpctl "sink"     volStep )
             , (xF86XK_Display,          ["arandr"])
             , (xF86XK_Favorites,        ["laptop.el"])
             , (xF86XK_ScreenSaver,      ["loginctl", "lock-session"])
             ]

    ++
    mapPairs ((noModMask,), safeSpawn "playerctl")
             [ (xF86XK_AudioNext, ["next"      ])
             , (xF86XK_AudioPlay, ["play-pause"])
             , (xF86XK_AudioPrev, ["previous"  ])
             ]

    ++
    mapPairs ((noModMask,), safeSpawn "playerctld")
             [ (xF86XK_Go, ["shift"  ])
             , (xK_Cancel, ["unshift"])
             ]

    ++
    mapPairs ((noModMask,), safeSpawn "blc-lux.el")
             [ (xF86XK_MonBrightnessDown, ["-s"])
             , (xF86XK_MonBrightnessUp,   [])
             ]

    ++
    mapPairs ((modMask',), safeSpawn')
             [ (xK_a, ["sensible-editor"    ])
             , (xK_b, ["blueman-manager"    ])
             , (xK_d, ["signal-desktop", "--use-tray-icon"])
             , (xK_f, ["nautilus"           ])
             , (xK_g, ["slack"              ])
             , (xK_o, ["passmenu"           ])
             , (xK_s, ["sensible-browser"   ])
             , (xK_v, ["pavucontrol"        ])
             , (xK_x, ["dunstctl", "context"])
             , (xK_y, ["spotify"            ])
             , (xK_z, ["dunstctl", "close"  ])
             ]

    ++
    mapPairs ((modMask' .|. shiftMask,), sendMessage)
             [ (xK_t, ToggleStruts)
             ]

    ++
    mapPairs ((modMask' .|. shiftMask,), safeSpawn')
             [ (xK_d, ["discord"                 ])
             , (xK_l, ["loginctl", "lock-session"])
             , (xK_o, ["passmenu", "--otp"       ])
             , (xK_s, ["sensible-browser", "-private-window", "--incognito"])
             , (xK_u, ["systemctl", "suspend"    ])
             , (xK_z, ["dunstctl", "history-pop" ])
             ]
