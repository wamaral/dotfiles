-----------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------

import XMonad hiding ( (|||) )

-- import Control.Applicative
-- import Data.Monoid
import System.Exit

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
-- import XMonad.Actions.Submap

import XMonad.Config.Xfce
-- import XMonad.Config.Desktop
-- import XMonad.Config.Gnome

import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

-- import XMonad.Layout.Drawer
-- import XMonad.Layout.HintedGrid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
-- import XMonad.Layout.TwoPane

import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
-- import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

-- import XMonad.Util.Dzen hiding (font)
-- import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Replace
import XMonad.Util.Run

import Graphics.X11.ExtraTypes.XF86

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/sbin/terminator"

-- Fonts and colors
myFont = "-*-calibri-normal-r-normal-*-12-*-*-*-c-*-*-*"
myIconDir = "/home/wamaral/.xmonad/dzen2"

-- Material color palette
-- https://www.google.com/design/spec/style/color.html#
black = "#000000"
white = "#FFFFFF"
grey = "#9E9E9E"
darkgrey = "#424242"
bluegrey = "#607D8B"
red = "#F44336"
darkred = "#B71C1C"
amber = "#FFC107"
green = "#4CAF50"
teal = "#009688"
indigo = "#3F51B5"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = False

-- Wether clicking a new desktop only focuses, or pass-through the click
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = clickable ["1.web", "2.dev", "3.dev", "4.servers", "5.servers", "6.misc", "7.misc", "8.misc", "9.misc", "0.scratch"]
  where clickable ws = [ "^ca(1,xdotool key super+" ++ show idx ++ ")" ++ name ++ "^ca()" |
                         (idx,name) <- zip [1..] ws]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    [ ((modm, xK_c), spawn $ XMonad.terminal conf) -- launch term
    , ((mod1Mask, xK_F5), kill) -- kill window
    , ((mod1Mask, xK_F4), kill1) -- close or kill window

      -- Runners
    , ((modm, xK_space), runOrRaisePrompt myXPConfig) -- app runner
    , ((modm .|. shiftMask, xK_space), xmonadPrompt myXPConfig) -- xmonad actions runner

      -- Media keys
    , ((0, xF86XK_AudioMute), spawn "pamixer -t")
    , ((0, xF86XK_AudioLowerVolume), spawn "pamixer --allow-boost -d 10")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pamixer --allow-boost -i 10")

      -- Printscreen
    , ((modm, xK_Print), spawn "scrot screen_%Y-%m-%d.png -d 1")
    , ((modm .|. shiftMask, xK_Print), spawn "bash -l -c select-screenshot")
    , ((modm .|. controlMask, xK_Print), spawn "imgur")

      -- Selecting windows
    , ((modm, xK_h), windows W.focusUp) -- move focus to the previous window
    , ((modm, xK_l), windows W.focusDown) -- move focus to the next window
    , ((modm, xK_m), dwmpromote) -- swap focused with master
    , ((modm, xK_Return), focusUrgent) -- move focus to urgent window
      -- Grid select
    , ((modm, xK_g), goToSelected myGSConfig)

      -- Swapping windows
    , ((modm .|. shiftMask, xK_h), windows W.swapUp) -- swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_l), windows W.swapDown)  -- swap the focused window with the next window
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster) -- swap the focused window and the master window

      -- Switching workspaces
    -- , ((modm, xK_Left), prevWS) -- switch to previous workspace
    -- , ((modm, xK_Right), nextWS) -- switch to next workspace
      -- Grid select
    , ((modm .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.view)

      -- Switching layouts
    , ((modm, xK_f), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_f), setLayout $ XMonad.layoutHook conf) -- Default workspace layout
    , ((modm, xK_F1), sendMessage $ JumpToLayout "Full")
    , ((modm, xK_F2), sendMessage $ JumpToLayout "RTile")
    , ((modm, xK_F3), sendMessage $ JumpToLayout "MirrorRTile")

      -- Resizing layouts
    , ((modm, xK_Left), sendMessage Shrink) -- shrink the master area
    , ((modm, xK_Right), sendMessage Expand) -- expand the master area
    , ((modm, xK_Down), sendMessage MirrorShrink) -- shrink the height/width
    , ((modm, xK_Up), sendMessage MirrorExpand) -- expand the height/width
      -- Number of windows in master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1)) -- increment
    , ((modm, xK_period), sendMessage (IncMasterN (-1))) -- decrement
    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)
      -- toggle the statusbar gap
    , ((modm, xK_b), sendMessage ToggleStruts)
      -- restart xfce panel
    , ((modm .|. shiftMask, xK_b), spawn "xfce4-panel -r")

      -- Floating
    , ((modm .|. controlMask, xK_Left), withFocused (keysMoveWindow (-30,0)))
    , ((modm .|. controlMask, xK_Right), withFocused (keysMoveWindow (30,0)))
    , ((modm .|. controlMask, xK_Up), withFocused (keysMoveWindow (0,-30)))
    , ((modm .|. controlMask, xK_Down), withFocused (keysMoveWindow (0,30)))
    -- Push window back into tiling
    , ((modm, xK_BackSpace), withFocused $ windows . W.sink)

      -- Xinerama screens
    , ((modm, xK_Tab), nextScreen) -- switch xinerama screens
    , ((modm .|. shiftMask, xK_Tab), shiftNextScreen >> nextScreen) -- window to next xinerama screen
    , ((modm .|. controlMask, xK_Tab), swapNextScreen >> nextScreen) -- swap xinerama screens

    -- Quit xmonad
    , ((modm .|. controlMask, xK_q), io exitSuccess)

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r), spawn "xmonad --restart")
    , ((modm .|. controlMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
    ]
    ++

    --
    -- mod-[1..0], Switch to workspace N
    --
    [((modm, k), toggleOrView i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])]
    ++

    --
    -- mod-shift-[1..0], Move client to workspace N
    --
    [((modm .|. shiftMask, k), windows $ W.shift i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])]
    ++

    --
    -- mod-ctrl-[1..0], Copy client to workspace N
    --
    [((modm .|. controlMask, k), windows $ copy i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])]
    ++

    --
    -- mod-{F10-11-12}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{F10-11-12}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_F10, xK_F11, xK_F12] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

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
-- Custom config options:
--

myStatusBar = "dzen2 -x '1280' -y '0' -h '20' -w '950' -ta 'l' -fg '" ++ white ++ "' -bg '" ++ black ++ "' -fn '" ++ myFont ++ "'"

myXPConfig = def
    { font = myFont
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

myGSConfig = def
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    , gs_font = myFont
    }


------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ layoutHints $ smartBorders
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
    ratio = toRational (2 / (1 + sqrt 5 :: Double))

    -- Percent of screen to increment by when resizing panes
    delta = 0.03

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll . concat $
    [ [ isDialog --> doFloat ]
    , [ className =? x --> doFloat | x <- floatByClass ]
    , [ title     =? x --> doFloat | x <- floatByTitle ]
    , [ resource  =? x --> doFloat | x <- floatByResource ]
    , [ className =? x --> doIgnore | x <- ignoreByClass ]
    , [ title     =? x --> doIgnore | x <- ignoreByTitle ]
    , [ resource  =? x --> doIgnore | x <- ignoreByResource ]
    , [ stringProperty "WM_WINDOW_ROLE" =? x --> doIgnore | x <- ignoreByResource ]
    , [ stringProperty "WM_NAME" =? x --> doFloat | x <- floatByTitle ]
    ] where
    floatByClass    = ["Ekiga", "MPlayer", "Nitrogen", "Skype", "Sysinfo", "XCalc", "XFontSel", "Xmessage", "Msjnc", "Hangouts", "mplayer2", "File-roller", "Gcalctool", "Exo-helper-1", "Gksu", "XClock", "Main", "wrapper-1.0", "Tiemu", "Xfce4-panel"]
    floatByTitle    = ["Downloads", "Iceweasel Preferences", "Save As...", "Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences", "Preferences", "Search Engines", "Set up sync", "Passwords and Exceptions", "Autofill Options", "Rename File", "Copying files", "Moving files", "File Properties", "Replace", "Quit GIMP", "Change Foreground Color", "Change Background Color", ""]
    floatByResource = ["pop-up", "presentationWidget"]
    ignoreByClass    = ["Xfce4-notifyd", "desktop_window"]
    ignoreByTitle    = []
    ignoreByResource = ["desktop", "desktop_window", "kdesktop"]

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
-- myEventHook _ = return (All True)
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = startupHook xfceConfig >> setWMName "LG3D"


------------------------------------------------------------------------
-- dynamicLog pretty printer for dzen:
--
myDzenPP h = def
    { ppCurrent = template white bluegrey -- xinerama current screen
    , ppVisible = template white grey -- xinerama other screen
    , ppHidden = template white darkgrey
    , ppHiddenNoWindows = \wsId -> if wsId `elem` staticWs
                                   then template black darkgrey wsId
                                   else ""
    , ppUrgent = template amber darkred
    , ppSep = "  "
    , ppWsSep = ""
    , ppTitle = wrap "λ" ""
    , ppLayout = dzenColor white black .
        (\x -> case x of
        "Hinted Full" -> icon "/layout_full.xbm"
        "Hinted RTile" -> icon "/layout_tall.xbm"
        "Hinted MirrorRTile" -> icon "/layout_mirror_tall.xbm"
        -- "Hinted MiddleTile" -> icon "/layout_tall.xbm"
        -- "Hinted MirrorMiddleTile" -> icon "/layout_mirror_tall.xbm"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
      staticWs = take 5 myWorkspaces
      icon path = "^fg(" ++ grey ++ ")^i(" ++ myIconDir ++ path ++ ")^fg()"
      template fg bg = wrap
        ("^ib(1)" -- ignore bg
         ++ "^fg(" ++ bg ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)" -- left corner
         ++ "^r(60x12)^p(-60)" -- rectangle
         ++ "^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")" -- color
         ++ "^p(2)λ") -- chevron
        ("^fg(" ++ bg ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)" -- right corner
         ++ "^ib(0)^fg()^bg()^p()") -- reset

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
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  replace
  myDzen <- spawnPipe myStatusBar
  --xmonad $ withUrgencyHook LibNotifyUrgencyHook $ ewmh xfceConfig {
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh xfceConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = black,
        focusedBorderColor = red,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook,
        handleEventHook    = myEventHook <+> docksEventHook,
        logHook            = dynamicLogWithPP $ myDzenPP myDzen,
        startupHook        = myStartupHook
    }
