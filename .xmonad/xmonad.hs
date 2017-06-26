{-# LANGUAGE TupleSections #-}

-- Base
import Control.Arrow                ( (***) )
import Data.Bits                    ( (.|.) )
import Data.List                    ( uncons )
import Data.Maybe                   ( fromMaybe )
import Text.Printf                  ( printf )

-- Third-party
import Data.Default                 ( def )
import Graphics.X11.Types           ( mod4Mask, noModMask, shiftMask, xK_a, xK_s
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
pamixer :: [String] -> X ()
pamixer args = do
    -- What if < 2 results?
    -- safeSpawn "osd_cat" (fmt <$> words <$> pamixer ...) ++ [...]
    [mute, vol] <- words <$> pa (["--allow-boost", "--get-mute", "--get-volume"] ++ args)
    return =<< safeSpawn "osd_cat" $ osdArgs mute vol
  where
    pa = flip (runProcessWithInput "pamixer") ""
    osdArgs mute vol
      = [ "--align=center"
        , "--barmode=percentage"
        , "--color=#8CD0D3"
        , "--delay=1"
        , "--font=-*-*-*-*-*-*-20-*-*-*-*-*-*-*"
        , "--outline=1"
        , "--percentage=" ++ vol
        , "--pos=bottom"
        , printf "--text=%3s%% %s" vol $ speaker' (mute == "true") (read vol)
        ]

main :: IO ()
main = xmonad $ additionalKeys def
     { borderWidth        = 1
     , focusedBorderColor = zenburn "blue" focusedBorderColor
     , focusFollowsMouse  = False
     , layoutHook         = avoidStruts  $  layoutHook def
     , manageHook         = manageDocks <+> manageHook def
     , modMask            = modMask'
     , normalBorderColor  = zenburn "bg-1" normalBorderColor
     , terminal           = "x-terminal-emulator"
     } $

     mapPairs ((noModMask,), pamixer)
              [ (xF86XK_AudioLowerVolume, [ "--decrease", volStep ])
              , (xF86XK_AudioRaiseVolume, [ "--increase", volStep ])
              , (xF86XK_AudioMute       , [ "--toggle-mute"       ])
              , (xF86XK_AudioMicMute    , [ "--toggle-mute"
                                          , "--default-source"    ])
              ]

     ++
     mapPairs ((modMask',), safeSpawn')
              [ (xK_a, ["sensible-editor" ])
              , (xK_s, ["sensible-browser"])
              ]

     ++
     mapPairs ((modMask' .|. shiftMask,), safeSpawn')
              [ (xK_s, ["sensible-browser", "-private-window", "--incognito"])
              ]

  where
    volStep     = "2"
    modMask'    = mod4Mask
    mapPairs    = map . uncurry (***)
    safeSpawn'  = maybe mempty (uncurry safeSpawn) . uncons
    zenburn k f = fromMaybe (f def) $ lookup k zenburnAlist
