{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           XMonad                           hiding ((|||))

import           System.Exit
import qualified Text.Fuzzy                       as Fuzzy

import qualified Data.Map                         as M
import qualified XMonad.Actions.FlexibleResize    as Flex
import qualified XMonad.StackSet                  as W

import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.GridSelect

-- import           XMonad.Config.Desktop
-- import           XMonad.Config.Gnome
-- import           XMonad.Config.Kde
-- import           XMonad.Config.Mate
import           XMonad.Config.Xfce

import           XMonad.Hooks.DynamicHooks
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook

import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile

import           XMonad.Prompt
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.XMonad

import           XMonad.Util.EZConfig
import           XMonad.Util.NamedWindows
import           XMonad.Util.Paste                (sendKey)
import           XMonad.Util.Replace
import           XMonad.Util.Run

import           System.Taffybar.Hooks.PagerHints (pagerHints)

import           Prompt.YubiOath                  (yubiOathPrompt)

------------------------------------------------------------------------
-- Custom config options
--
defaultFont :: String
defaultFont = "-*-calibri-normal-r-normal-*-14-*-*-*-c-*-*-*"

-- workspaceNames = ["1.web", "2.dev", "3.dev", "4.servers", "5.servers", "6.misc", "7.misc", "8.misc", "9.misc", "0.chat"]
workspaceNames :: [Integer]
workspaceNames = [1..8]


-- Material color palette
-- https://www.google.com/design/spec/style/color.html#
black = "#000000"
white = "#FFFFFF"
-- grey = "#9E9E9E"
-- darkgrey = "#424242"
-- bluegrey = "#607D8B"
red = "#F44336"
darkred = "#B71C1C"
-- amber = "#FFC107"
-- green = "#4CAF50"
-- teal = "#009688"
-- indigo = "#3F51B5"

-- dzenStatus = " -fg '" ++ white ++ "' -bg '" ++ black ++ "' -fn '" ++ defaultFont ++ "'"
-- statusBar = "dzen2 -x '1280' -y '0' -h '20' -w '950' -ta 'l'" ++ dzenStatus
-- conky = "conky -c ~/.xmonad/conkybarrc"

xpConfig = def
    { font = defaultFont
    , fgColor = white
    , bgColor = black
    , fgHLight = white
    , bgHLight = darkred
    , borderColor = red
    , promptBorderWidth = 1
    , position = Bottom
    , height = 20
    , historySize = 100
    }

fuzzyXpConfig = xpConfig { searchPredicate = Fuzzy.test }

gsConfig = def
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    , gs_font = defaultFont
    }

------------------------------------------------------------------------
-- Key bindings
--
fKeys :: [KeySym]
fKeys = [xK_F1 .. xK_F12]

keymapConfig conf = mkKeymap conf $
    [ ("M4-c", spawn $ XMonad.terminal conf) -- launch term
    , ("M1-<F5>", kill) -- kill window
    , ("M1-<F4>", kill1) -- close or kill window

      -- Runners
    , ("M4-<Space>", runOrRaisePrompt xpConfig) -- app runner
    , ("M4-S-<Space>", xmonadPrompt fuzzyXpConfig) -- xmonad actions runner
    , ("M4-w", spawn "keepass --auto-type") -- retrieve pass entries
    , ("M4-y", yubiOathPrompt fuzzyXpConfig) -- Yubikey OATH auto type

      -- Media keys
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPlayPause>", spawn "playerctl play-pause")
    , ("<XF86AudioMute>", spawn "pamixer -t")
    , ("<XF86AudioLowerVolume>", spawn "pamixer --allow-boost -d 5")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer --allow-boost -i 5")

      -- Printscreen
    , ("M4-<Print>", spawn "scrot screen_%Y-%m-%d.png -d 1")
    , ("M4-S-<Print>", spawn "bash -l -c select-screenshot")
    , ("M4-C-<Print>", spawn "imgur")

      -- Selecting windows
    , ("M4-h", windows W.focusUp) -- move focus to the previous window
    , ("M4-l", windows W.focusDown) -- move focus to the next window
    , ("M4-m", dwmpromote) -- swap focused with master
    , ("M4-<Return>", focusUrgent) -- move focus to urgent window
      -- Grid select
    , ("M4-g", goToSelected gsConfig)

      -- Swapping windows
    , ("M4-S-h", windows W.swapUp) -- swap the focused window with the previous window
    , ("M4-S-l", windows W.swapDown)  -- swap the focused window with the next window
    , ("M4-S-m", windows W.swapMaster) -- swap the focused window and the master window

      -- Switching workspaces
    -- , ("M4-<Left>", prevWS) -- switch to previous workspace
    -- , ("M4-<Right>", nextWS) -- switch to next workspace
      -- Grid select
    , ("M4-S-g", gridselectWorkspace gsConfig W.view)

      -- Switching layouts
    , ("M4-f", sendMessage NextLayout)
    , ("M4-S-f", setLayout $ XMonad.layoutHook conf) -- Default workspace layout
    -- , ("M4-<F1>", sendMessage $ JumpToLayout "Full")
    -- , ("M4-<F2>", sendMessage $ JumpToLayout "RTile")
    -- , ("M4-<F3>", sendMessage $ JumpToLayout "MirrorRTile")
    , ("<F9>", sendMessage $ JumpToLayout "Full")
    , ("<F10>", sendMessage $ JumpToLayout "RTile")
    , ("<F11>", sendMessage $ JumpToLayout "MirrorRTile")

      -- Resizing layouts
    , ("M4-<Left>", sendMessage Shrink) -- shrink the master area
    , ("M4-<Right>", sendMessage Expand) -- expand the master area
    , ("M4-<Down>", sendMessage MirrorShrink) -- shrink the height/width
    , ("M4-<Up>", sendMessage MirrorExpand) -- expand the height/width
      -- Number of windows in master area
    , ("M4-,", sendMessage (IncMasterN 1)) -- increment
    , ("M4-.", sendMessage (IncMasterN (-1))) -- decrement
    -- Resize viewed windows to the correct size
    , ("M4-n", refresh)
      -- toggle the statusbar gap
    , ("M4-b", sendMessage ToggleStruts)
      -- restart mate panel
    , ("M4-S-b", spawn "mate-panel --replace")
    , ("M4-C-b", spawn "mate-panel --replace")

      -- Floating
    , ("M4-S-<Left>", withFocused (keysMoveWindow (-30,0)))
    , ("M4-S-<Right>", withFocused (keysMoveWindow (30,0)))
    , ("M4-S-<Up>", withFocused (keysMoveWindow (0,-30)))
    , ("M4-S-<Down>", withFocused (keysMoveWindow (0,30)))
    -- Push window back into tiling
    , ("M4-<Backspace>", withFocused $ windows . W.sink)

      -- Xinerama screens
    , ("M4-<Tab>", nextScreen) -- switch xinerama screens
    , ("M4-S-<Tab>", shiftNextScreen >> nextScreen) -- window to next xinerama screen
    , ("M4-C-<Tab>", swapNextScreen >> nextScreen) -- swap xinerama screens

    -- Quit xmonad
    , ("M4-C-q", io exitSuccess)

    -- Restart xmonad
    , ("M4-S-r", spawn "xmonad --restart")
    , ("M4-C-r", spawn "xmonad --recompile && xmonad --restart")

    -- Screensaver
    -- , ("M4-<Esc>", spawn "xscreensaver-command -lock")

    -- Screens
    , ("M4-a", toggleOrView $ last $ XMonad.workspaces conf)
    ]
    ++

    --
    -- F[1..8], Switch to workspace N
    --
    [("M4-<F" ++ show n ++ ">", sendKey noModMask fk)
    | (n, fk) <- zip ([1..12] :: [Integer]) fKeys]
    ++
    [("<F" ++ show k ++ ">", toggleOrView i)
    | (i, k) <- zip (XMonad.workspaces conf) workspaceNames]
    ++

    --
    -- shift-[1..8], Move client to workspace N
    --
    -- [("M4-S-<F" ++ show n ++ ">", sendKey shiftMask fk)
    -- | (n, fk) <- zip [1..12] fKeys]
    -- ++
    [("S-<F" ++ show k ++ ">", windows $ W.shift i)
        | (i, k) <- zip (XMonad.workspaces conf) workspaceNames]
    ++

    --
    -- ctrl-[1..8], Copy client to workspace N
    --
    -- [("M4-C-<F" ++ show n ++ ">", sendKey controlMask fk)
    -- | (n, fk) <- zip [1..12] fKeys]
    -- ++
    [("C-<F" ++ show k ++ ">", windows $ copy i)
        | (i, k) <- zip (XMonad.workspaces conf) workspaceNames]
    -- ++

    -- --
    -- -- mod-{F10-11-12}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{F10-11-12}, Move client to screen 1, 2, or 3
    -- --
    -- [(m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip ["<F10>", "<F11>", "<F12>"] [0..]
    --     , (f, m) <- [(W.view, "M4-"), (W.shift, "M4-S-")]]

------------------------------------------------------------------------
-- Mouse bindings
--
mouseConfig XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    -- , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button2 (middle-mouse), Float window and store xprop info
    , ((modm, button2), \w -> spawn "/home/wamaral/bin/xmonadpropclick" >> focus w >> float w)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modm, button4), const prevWS) -- switch to previous workspace
    , ((modm, button5), const nextWS) -- switch to next workspace
    ]

------------------------------------------------------------------------
-- Layouts
--
layoutConfig = avoidStruts $ layoutHints $ smartBorders
     (rTile ||| rMirTile ||| Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    -- tiled = ResizableTall nmaster delta 1/2

    -- myMiddleTile = ResizableTall nmaster delta 0.5 []
    -- myMirrorMiddleTile = Mirror myMiddleTile
    -- rMidTile = renamed [Replace "MiddleTile"] myMiddleTile
    -- rMirMidTile = renamed [Replace "MirrorMiddleTile"] myMirrorMiddleTile

    -- resizable version
    myResizableTile = ResizableTall nmaster delta ratio []
    myMirrorResizableTile = Mirror myResizableTile
    rTile = renamed [Replace "RTile"] myResizableTile
    rMirTile = renamed [Replace "MirrorRTile"] myMirrorResizableTile

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    -- ratio = toRational (2 / (1 + sqrt 5 :: Double))
    ratio = 0.5

    -- Percent of screen to increment by when resizing panes
    delta = 0.03

--------------------------------------------------------------------
-- Window rules
-- Use xprop to find this information
--
floatByClass    = ["Ekiga", "MPlayer", "Nitrogen", "Skype", "Sysinfo", "XCalc", "XFontSel", "Xmessage", "Msjnc", "Hangouts", "mplayer2", "File-roller", "Gcalctool", "Exo-helper-1", "Gksu", "XClock", "Main", "wrapper-1.0", "Tiemu", "Xfce4-panel", "Evolution", "Claws-mail", "Orage", "KeePass.exe", "KeePass2", "keepassxc", "Syncthing GTK", "Plasma-desktop", "xfce4-appfinder"]
floatByTitle    = ["Downloads", "Iceweasel Preferences", "Save As...", "Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences", "Preferences", "Search Engines", "Set up sync", "Passwords and Exceptions", "Autofill Options", "Rename File", "Copying files", "Moving files", "File Properties", "Replace", "Quit GIMP", "Change Foreground Color", "Change Background Color", "chrome://pourbico - Tracker :: PourBico - Firefox Developer Edition", "Syncthing-GTK", ""]
floatByResource  = ["pop-up", "presentationWidget"]
ignoreByClass    = ["Xfce4-notifyd", "desktop_window"]
ignoreByTitle    = []
ignoreByResource = ["desktop", "desktop_window", "kdesktop"]

manageHookConfig = composeAll . concat $
    [ [ isDialog --> doFloat ]
    , [ className =? x --> doFloat | x <- floatByClass ]
    , [ title     =? x --> doFloat | x <- floatByTitle ]
    , [ resource  =? x --> doFloat | x <- floatByResource ]
    , [ className =? x --> doIgnore | x <- ignoreByClass ]
    , [ title     =? x --> doIgnore | x <- ignoreByTitle ]
    , [ resource  =? x --> doIgnore | x <- ignoreByResource ]
    , [ stringProperty "WM_WINDOW_ROLE" =? x --> doIgnore | x <- ignoreByResource ]
    , [ stringProperty "WM_NAME" =? x --> doFloat | x <- floatByTitle ]
    ]

------------------------------------------------------------------------
-- Startup hook
--
startupHookConfig = startupHook xfceConfig >> setWMName "LG3D"

------------------------------------------------------------------------
-- UrgencyHook via libnotify
--
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    Just idx <- W.findTag w <$> gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

------------------------------------------------------------------------
-- Run
--
userConfig = xfceConfig { terminal           = "/usr/bin/termite"
                        , focusFollowsMouse  = False
                        -- Wether clicking a new desktop only focuses, or pass-through the click
                        , clickJustFocuses   = False
                        -- Width of the window border in pixels
                        , borderWidth        = 2
                        -- mod1Mask = left alt; mod2Mask = right alt; mod4Mask = win key
                        , modMask            = mod4Mask
                        , workspaces         = map show workspaceNames
                        , normalBorderColor  = black
                        , focusedBorderColor = red
                        , keys               = keymapConfig
                        , mouseBindings      = mouseConfig
                        , layoutHook         = layoutConfig
                        , manageHook         = manageHookConfig <+> manageDocks <+> dynamicMasterHook
                        , handleEventHook    = mempty <+> docksEventHook
                        , startupHook        = startupHookConfig
                        }

main :: IO ()
main = do
  replace
  xmonad $ withUrgencyHook LibNotifyUrgencyHook $ pagerHints userConfig
