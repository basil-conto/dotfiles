import qualified Data.Text as T

import System.Taffybar.SimpleConfig         ( SimpleTaffyConfig(..)
                                            , defaultSimpleTaffyConfig
                                            , simpleTaffybar )
import System.Taffybar.Widget.Battery       ( textBatteryNew )
import System.Taffybar.Widget.CommandRunner ( commandRunnerNew )
import System.Taffybar.Widget.Layout        ( defaultLayoutConfig, layoutNew )
import System.Taffybar.Widget.MPRIS2        ( mpris2New )
import System.Taffybar.Widget.SNITray       ( sniTrayNew )
import System.Taffybar.Widget.SimpleClock   ( textClockNew )
import System.Taffybar.Widget.Windows       ( defaultWindowsConfig, windowsNew )
import System.Taffybar.Widget.Workspaces    ( WorkspacesConfig(..)
                                            , defaultWorkspacesConfig
                                            , hideEmpty, workspacesNew )

main :: IO ()
main = do
  let secs = 10.0
      batt = textBatteryNew "$percentage$%"
      clck = textClockNew Nothing "%F %a %R %z" secs
      load = commandRunnerNew 10.0 "cut" ["-c-4", "/proc/loadavg"] T.empty
      lout = layoutNew defaultLayoutConfig
      wins = windowsNew defaultWindowsConfig
      work = workspacesNew defaultWorkspacesConfig
           { borderWidth      = 0
           , showWorkspaceFn  = hideEmpty
           , underlineHeight  = 0
           , underlinePadding = 0
           }
  simpleTaffybar defaultSimpleTaffyConfig
                 { barHeight    = 22
                 , endWidgets   = [ clck, batt, load, sniTrayNew, mpris2New ]
                 , startWidgets = [ work, lout, wins ]
                 }
