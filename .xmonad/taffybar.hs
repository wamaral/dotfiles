import           System.Taffybar

-- import System.Information.Battery
import           System.Information.CPU
-- import System.Information.CPU2
import           System.Information.DiskIO
-- import System.Information.EWMHDesktopInfo
import           System.Information.Memory
-- import System.Information.Network
-- import System.Information.StreamInfo
-- import System.Information.X11DesktopInfo

-- import System.Taffybar.Battery
-- import System.Taffybar.CPUMonitor
-- import System.Taffybar.CommandRunner
-- import System.Taffybar.DiskIOMonitor
-- import System.Taffybar.FSMonitor
import           System.Taffybar.FreedesktopNotifications

-- import System.Taffybar.Hooks.PagerHints
-- import System.Taffybar.LayoutSwitcher
import           System.Taffybar.MPRIS
-- import System.Taffybar.MPRIS2
import           System.Taffybar.NetMonitor
import           System.Taffybar.Pager                    (colorize, escape,
                                                           wrap)
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray
import           System.Taffybar.TaffyPager

-- import System.Taffybar.Text.CPUMonitor
-- import System.Taffybar.Text.MemoryMonitor

import           System.Taffybar.Weather
import           System.Taffybar.Widgets.Graph
-- import System.Taffybar.Widgets.PollingBar
import           System.Taffybar.Widgets.PollingGraph
-- import System.Taffybar.Widgets.PollingLabel
-- import System.Taffybar.Widgets.Util
-- import System.Taffybar.Widgets.VerticalBar
-- import System.Taffybar.WindowSwitcher
-- import System.Taffybar.WorkspaceSwitcher

myShorten :: Int -> String -> String
myShorten len str
  | length str <= len = str
  | len > 1           = take (len - 1) str ++ "…"
  | otherwise         = "…"

data ColourType = Default | Active | Urgent | Empty

colour :: ColourType -> String -> String
colour t = case t of
  Default -> id
  Active  -> colorize "yellow" ""
  Urgent  -> colorize "red" "yellow"
  Empty   -> colorize "grey" ""

layout :: String -> String
layout s = case s of
  "Hinted Full"        -> "◻"
  "Hinted RTile"       -> "◫"
  "Hinted MirrorRTile" -> "⬓"
  _                    -> s

emptyWs :: String -> String
emptyWs ws
  | [] == ws       = ""
  | head ws == '0' = ""
  | head ws < '6'  = ws
  | otherwise      = ""

bold :: String -> String
bold s
  | [] == s   = s
  | otherwise = wrap "<b>" "</b>" s

strike :: String -> String
strike s
  | [] == s   = s
  | otherwise = wrap "<s>" "</s>" s

brace :: String -> String
brace = wrap "[" "]"

pad :: String -> String
pad s
  | [] == s   = s
  | otherwise = wrap " " " " s

smaller :: String -> String
smaller s
  | [] == s   = s
  | otherwise = wrap "<small>" "</small>" s

bigger :: String -> String
bigger s
  | [] == s   = s
  | otherwise = wrap "<big>" "</big>" s

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

diskCallback :: IO [Double]
diskCallback = do
  [r, w] <- getDiskTransfer "sda"
  return [r, w]

main :: IO ()
main = do
  let cfg = defaultTaffybarConfig { barHeight = 24
                                  , barPosition = Top
                                  , widgetSpacing = 8
                                  , monitorNumber = 1
                                  }
      memCfg = defaultGraphConfig { graphDataColors = [ (0, 0.5, 1, 1) ]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
      diskCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                       , (0.7, 0, 0, 1)
                                                       ]
                                   , graphLabel = Just "disk"
                                   }
      pagerCfg = PagerConfig { activeWindow     = pad . escape . myShorten 150
                             , activeLayout     = pad . layout
                             , activeWorkspace  = colour Active . bold . brace . escape
                             , hiddenWorkspace  = colour Default . escape
                             , emptyWorkspace   = colour Empty . escape
                             , visibleWorkspace = colour Default . bold . brace . escape
                             , urgentWorkspace  = colour Urgent . escape
                             , widgetSep        = " λ "
                             }
      weatherCfg = (defaultWeatherConfig "SBSP") { weatherTemplate = "$tempC$°C " ++ (smaller . smaller $ "$humidity$%") }

      clock = textClockNew Nothing (smaller "%a %F" ++ " <span fgcolor='orange'><b>%T</b></span>") 1
      pager = taffyPagerNew pagerCfg

      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew weatherCfg 10
      mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      disk = pollingGraphNew diskCfg 1 diskCallback
      net = netMonitorNew 1 "wlp2s0"
      tray = systrayNew

  -- defaultTaffybar cfg { startWidgets = [ pager, note ]
  --                     , endWidgets = [ clock, tray, wea, mem, cpu, disk, net, mpris ]
  --                     }
  defaultTaffybar cfg { startWidgets = [ pager, note ]
                      , endWidgets = [ tray, mpris ]
                      }
