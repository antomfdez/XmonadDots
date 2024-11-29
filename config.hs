{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Layout.Spacing
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Util.ClickableWorkspaces
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.ToggleFullFloat

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.NoBorders (toggleBorder)

myTerminal :: String
myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth   = 1

myModMask :: KeyMask
myModMask       = mod1Mask

myWorkspaces :: [String]
myWorkspaces    = ["web", "code", "ctrl", "term", "doc", "virt", "test", "files", "other" ]

myNormalBorderColor :: String
myNormalBorderColor  = "#000000"
myFocusedBorderColor :: String
myFocusedBorderColor = "#ff6c6b"

-- ################################
-- ||                            ||
-- ||           binds            ||
-- ||                            ||
-- ################################

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    , ((modm,               xK_p     ), spawn "dmenu_run")

    , ((modm,               xK_f     ), spawn "flameshot gui")

    , ((modm .|. shiftMask, xK_c     ), kill)

    , ((modm,               xK_space ), sendMessage NextLayout)

    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm,               xK_n     ), refresh)

    , ((modm,               xK_Tab   ), windows W.focusDown)

    , ((modm,               xK_j     ), windows W.focusDown)

    , ((modm,               xK_k     ), windows W.focusUp  )

    , ((modm,               xK_m     ), windows W.focusMaster  )

    , ((modm,               xK_Return), windows W.swapMaster)

    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modm,               xK_h     ), sendMessage Shrink)

    , ((modm,               xK_l     ), sendMessage Expand)

    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    , ((modm              , xK_q     ), spawn "rm -rf ~/.xmonad/dist-newstyle; xmonad --recompile; pkill xmobar; ~/.xmonad/xmonad-x86_64-freebsd --restart")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- ################################
-- ||                            ||
-- ||           layout           ||
-- ||                            ||
-- ################################

myLayout = smartSpacingWithEdge 10 (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- ################################
-- ||                            ||
-- ||           hooks            ||
-- ||                            ||
-- ################################

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen -->  doFullFloat ]


myEventHook = windowedFullscreenFixEventHook


myLogHook :: X ()
myLogHook = return ()


myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xrandr --output HDMI-0 --mode 1920x1080 --rate 144"
  spawnOnce "picom --config ~/.xmonad/picom/picom.conf &"
  spawnOnce "~/.fehbg &"
  spawnOnce "flameshot &"

-- ################################
-- ||                            ||
-- ||             PP             ||
-- ||                            ||
-- ################################

myPPminimal = def
   { ppLayout = const ""  -- Don't show the layout name
   , ppTitle = const ""  -- Don't show the focused window's title
   , ppTitleSanitize = const ""  -- Also about window's title
   , ppCurrent = xmobarColor "#fffff0" "" . wrap "[ " " ]"
   , ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!"
   , ppHidden = xmobarColor "#c678dd" "" . wrap "" ""
   , ppHiddenNoWindows = xmobarColor "#61afef" "" . wrap "" ""
   , ppWsSep = "  "
   }

-- ################################
-- \|                            ||
-- ||            main            ||
-- ||                            ||
-- ################################

mySB = statusBarProp "xmobar -x 0 ~/.xmonad/xmobar/xmobarrc" (pure myPPminimal)
main = do
  xmonad
  $ toggleFullFloatEwmhFullscreen . ewmhFullscreen . ewmh
  $ withEasySB mySB defToggleStrutsKey defaults

-- ################################
-- ||                            ||
-- ||           config           ||
-- ||                            ||
-- ################################

defaults = def {
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
        layoutHook         = smartBorders myLayout,
        manageHook         = manageSpawn <> manageHook def,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }