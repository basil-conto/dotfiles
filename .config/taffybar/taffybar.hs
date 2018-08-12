import System.Taffybar                     ( TaffybarConfig(..), defaultTaffybar
                                           , defaultTaffybarConfig )
import System.Taffybar.Battery             ( batteryBarNew )
import System.Taffybar.CommandRunner       ( commandRunnerNew )
import System.Taffybar.MPRIS2              ( mpris2New )
import System.Taffybar.Pager               ( defaultPagerConfig )
import System.Taffybar.SimpleClock         ( textClockNew )
import System.Taffybar.Systray             ( systrayNew )
import System.Taffybar.TaffyPager          ( taffyPagerNew )
import System.Taffybar.Widgets.VerticalBar ( defaultBarConfig )

main :: IO ()
main = do
  let secs  = 10.0
      clock = textClockNew Nothing "%F %a %R %z" secs
      load  = commandRunnerNew (secs / 2.0)
                               "sh" ["-c", "cat /proc/loadavg | cut -c-4"]
                               ""
                               "white"
      batt  = batteryBarNew (defaultBarConfig colour) secs
      pager = taffyPagerNew defaultPagerConfig
  defaultTaffybar defaultTaffybarConfig
                    { barHeight    = 18
                    , endWidgets   = [ clock, load, batt, systrayNew, mpris2New ]
                    , startWidgets = [ pager ]
                    }
  where
    colour p | p < 0.2   = (1.0, 0.0, 0.0)
             | p < 0.8   = (0.5, 0.5, 0.5)
             | otherwise = (0.0, 1.0, 0.0)
