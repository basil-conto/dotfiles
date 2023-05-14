import System.Taffybar.SimpleConfig           ( SimpleTaffyConfig(..)
                                              , StrutSize(ExactSize)
                                              , defaultSimpleTaffyConfig
                                              , simpleTaffybar )
import System.Taffybar.Widget.Battery         ( batteryIconNew, textBatteryNew )
import System.Taffybar.Widget.Layout          ( defaultLayoutConfig, layoutNew )
import System.Taffybar.Widget.SNITray         ( sniTrayNew )
import System.Taffybar.Widget.SimpleClock     ( textClockNew )
import System.Taffybar.Widget.Text.CPUMonitor ( textCpuMonitorNew )
import System.Taffybar.Widget.Windows         ( defaultWindowsConfig
                                              , windowsNew )
import System.Taffybar.Widget.Workspaces      ( WorkspacesConfig(..)
                                              , defaultWorkspacesConfig
                                              , hideEmpty, workspacesNew )


main :: IO ()
main = do
  let secs = 10.0
      batt = textBatteryNew "$percentage$%"
      clck = textClockNew Nothing "%F %a %R %z" secs
      load = textCpuMonitorNew "$total$%" secs
      lout = layoutNew defaultLayoutConfig
      wins = windowsNew defaultWindowsConfig
      work = workspacesNew defaultWorkspacesConfig
           { borderWidth      = 0
           , showWorkspaceFn  = hideEmpty
           }
  simpleTaffybar defaultSimpleTaffyConfig
                 { barHeight    = ExactSize 22
                 , endWidgets   = [ clck, batt, batteryIconNew , sniTrayNew
                                  , load ]
                 , startWidgets = [ work, lout, wins ]
                 }
