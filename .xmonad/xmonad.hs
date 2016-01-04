-----------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------

import XMonad hiding ( (|||) )

import Control.Applicative
import Data.Monoid
import System.Exit

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect

import XMonad.Config.Xfce
-- import XMonad.Config.Desktop
-- import XMonad.Config.Gnome

import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

import XMonad.Util.NamedWindows
import XMonad.Util.Run

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "/usr/bin/sakura"

-- Fonts and colors
myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
myIconDir = "/home/wamaral/.xmonad/dzen2"
-- general
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
-- dzenpp
myDzenFGColor = "#555555"
myIconFGColor = "#777777"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#555555"
myPatternColor = "#1f1f1f"
-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#af1f1f"

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
myWorkspaces = ["1:chat", "2:web", "3:dev", "4:dev", "5:servers", "6:misc", "7:misc", "8:misc", "9:misc", "0:scratch"]


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_c), spawn $ XMonad.terminal conf) -- launch term
    , ((mod1Mask, xK_F4), kill) -- kill window

      -- Runners
    , ((modm, xK_space), shellPrompt myXPConfig) -- app runner
    , ((modm .|. shiftMask, xK_space), xmonadPrompt myXPConfig) -- xmonad actions runner

      -- Printscreen
    , ((modm, xK_Print), spawn "scrot screen_%Y-%m-%d.png -d 1")
    , ((modm .|. shiftMask, xK_Print), spawn "bash -l -c select-screenshot")
    , ((modm .|. controlMask, xK_Print), spawn "imgur")

      -- Selecting windows
    , ((modm, xK_h), windows W.focusUp) -- move focus to the previous window
    , ((modm, xK_l), windows W.focusDown) -- move focus to the next window
    , ((modm, xK_m), windows W.focusMaster) -- move focus to the master window
    , ((modm, xK_Return), focusUrgent) -- move focus to urgent window
      -- Grid select
    , ((modm, xK_g), goToSelected myGSConfig)

      -- Swapping windows
    , ((modm .|. shiftMask, xK_h), windows W.swapUp) -- swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_l), windows W.swapDown)  -- swap the focused window with the next window
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster) -- swap the focused window and the master window

      -- Switching workspaces
    , ((modm, xK_Left), prevWS) -- switch to previous workspace
    , ((modm, xK_Right), nextWS) -- switch to next workspace
      -- Grid select
    , ((modm .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.view)

      -- Switching layouts
    , ((modm, xK_f), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_f), setLayout $ XMonad.layoutHook conf) -- Default workspace layout
    , ((modm, xK_F1), sendMessage $ JumpToLayout "Full")
    , ((modm, xK_F2), sendMessage $ JumpToLayout "ResizableTall")
    , ((modm, xK_F3), sendMessage $ JumpToLayout "Mirror ResizableTall")

      -- Resizing layouts
    , ((modm .|. shiftMask, xK_j), sendMessage Shrink) -- shrink the master area
    , ((modm .|. shiftMask, xK_k), sendMessage Expand) -- expand the master area
    , ((modm .|. controlMask, xK_j), sendMessage MirrorShrink) -- shrink the height/width
    , ((modm .|. controlMask, xK_k), sendMessage MirrorExpand) -- expand the height/width
      -- Number of windows in master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1)) -- increment
    , ((modm, xK_period), sendMessage (IncMasterN (-1))) -- decrement
    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)
      -- toggle the statusbar gap
    , ((modm, xK_b), sendMessage ToggleStruts)

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
    -- mod-{F10-11-12}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{F10-11-12}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_F10, xK_F11, xK_F12] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modm, button4), const prevWS) -- switch to previous workspace
    , ((modm, button5), const nextWS) -- switch to next workspace
    ]


------------------------------------------------------------------------
-- Custom config options:
--

myStatusBar = "dzen2 -x '0' -y '0' -h '20' -w '950' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"

myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 1
    , position = Bottom
    , height = 20
    , historySize = 100
    }

myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    , gs_font = "" ++ myFont ++ ""
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
     (Full |||  resizableTile ||| Mirror resizableTile)
  where
    -- default tiling algorithm partitions the screen into two panes
    -- tiled   = Tall nmaster delta 1/2

    -- resizable version
    resizableTile = ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = toRational (2 / (1 + sqrt 5 :: Double))

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

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
    , [ stringProperty "WM_WINDOW_ROLE" =? x --> doIgnore | x <- floatByResource ]
    ] where
    floatByClass    = ["Ekiga", "MPlayer", "Nitrogen", "Skype", "Sysinfo", "XCalc", "XFontSel", "Xmessage", "Msjnc", "Hangouts"]
    floatByTitle    = ["Downloads", "Iceweasel Preferences", "Save As..."]
    floatByResource = ["pop-up", "presentationWidget"]
    ignoreByClass    = ["Xfce4-notifyd"]
    ignoreByTitle    = []
    ignoreByResource = ["desktop_window", "kdesktop"]

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
myEventHook _ = return (All True)

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
myStartupHook = return ()


------------------------------------------------------------------------
-- dynamicLog pretty printer for dzen:
--
myDzenPP h = defaultPP
    { ppCurrent = currentFmt . \wsId -> dropIx wsId
    , ppVisible = visibleFmt . \wsId -> dropIx wsId
    , ppHidden = hiddenFmt . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs
                                   then ""
                                   else hiddenNoWinFmt . dropIx $ wsId
    , ppUrgent = urgentFmt . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = ""
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . titleFmt
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout_full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout_tall.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout_mirror_tall.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
      currentFmt = wrap
                   ("^p(2)^ib(1)^fg(" ++ myFocusedBGColor
                    ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)"
                    ++ "^r(1300x12)^p(-1300)^fg(" ++ myFocusedFGColor
                    ++ ")^bg(" ++ myFocusedBGColor
                    ++ ")^p()``^fg(" ++ myNormalFGColor
                    ++ ")^p(2)")
                   ("^p(2)^fg(" ++ myFocusedBGColor
                    ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)"
                    ++ "^fg(" ++ myNormalBGColor
                    ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()")
      visibleFmt = wrap
                  ("^p(2)^ib(1)^fg(" ++ myPatternColor
                   ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)"
                   ++ "^r(1300x12)^p(-1300)^fg(" ++ myNormalFGColor
                   ++ ")^bg(" ++ myFocusedBGColor
                   ++ ")^p()``^fg(" ++ myNormalFGColor ++ ")^p(2)")
                  ("^p(2)^fg(" ++ myPatternColor
                   ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)"
                   ++ "^fg(" ++ myNormalBGColor
                   ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()")
      hiddenFmt = wrap
                  ("^p(2)^ib(1)^fg(" ++ myPatternColor
                   ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)"
                   ++ "^r(1300x12)^p(-1300)^fg()^bg()^p()``^p(2)")
                  ("^p(2)^fg(" ++ myPatternColor
                   ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)"
                   ++ "^fg(" ++ myNormalBGColor
                   ++ ")^r(1300x12)^p(-1300)^p()^ib(0)^fg()^bg()^p()")
      hiddenNoWinFmt = wrap
                       ("^p(2)^ib(1)^fg(" ++ myPatternColor
                        ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)"
                        ++ "^r(1300x12)^p(-1300)^fg(" ++ myDzenFGColor
                        ++ ")^bg()^p()``^p(2)")
                       ("^p(2)^fg(" ++ myPatternColor
                        ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)"
                        ++ "^fg(" ++ myNormalBGColor
                        ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()")
      urgentFmt = wrap
                  ("^p(2)^ib(1)^fg(" ++ myPatternColor
                   ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)"
                   ++ "^r(1300x12)^p(-1300)^fg(" ++ myUrgentFGColor
                   ++ ")^bg(" ++ myUrgentBGColor
                   ++ ")^p()``^fg(" ++ myUrgentFGColor ++ ")^p(2)")
                  ("^p(2)^fg(" ++ myPatternColor
                   ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)"
                   ++ "^fg(" ++ myNormalBGColor
                   ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()")
      titleFmt = wrap
                 ("^ib(1)^fg(" ++ myPatternColor
                  ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)"
                  ++ "^r(1300x12)^p(-1300)^p(2)^fg()< ")
                 (" >^p(2)^fg(" ++ myPatternColor
                  ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)"
                  ++ "^fg(" ++ myNormalBGColor
                  ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()")
      dropIx wsId = wsId -- if ':' `elem` wsId then drop 2 wsId else wsId
      staticWs = take 5 myWorkspaces


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
  myDzen <- spawnPipe myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ xfceConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

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
