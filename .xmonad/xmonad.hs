-- look at DynamicWorkspaceGroups
-- should enable hotkeys between entire sets of workspaces

import Data.Ratio ((%))
import Data.Monoid
import System.Exit

-- our topicspace config
import Topics.Topics
-- downloaded from cabal
--import Actions.DynamicWorkspaceGroups

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.NoBorders
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.IM
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare
-- import XMonad.Util.EZConfig
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

myModMask  = mod1Mask
myShell    = "zsh"
myTerminal = "sakura"

myScreenshotPrompt :: XPConfig -> X ()
myScreenshotPrompt c = inputPrompt c "Screenshot name" ?+ \name -> io $ spawn ("scrot -s /tmp/" ++ name ++ ".png")

------------------------------------------------------------------------
-- xprop, then click on window of interest
-- WM_CLASS = className
-- WM_NAME = title

myManageHook = composeAll
    [ className =? "Pidgin"         --> doShift "2_home"
    --, className =? "Gimp"           --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]

------------------------------------------------------------------------

myLayouts = avoidStruts
            $ spacingWithEdge 6
            $ smartBorders
            $ onWorkspaces ["2_home"] (imLayout)
            $ mkToggle (single REFLECTX)
            $ baseLayouts
            where
              baseLayouts     = maximize (gridLayout ||| threeColLayout)
              gridLayout      = GridRatio (3/2)
              threeColLayout  = ThreeColMid 1 (3/100) (1/3)
              imLayout        = reflectHoriz $ withIM (1%9) (Role "buddy_list") baseLayouts

------------------------------------------------------------------------

myStartupHook = do
  -- setup ssh agent on xmonad start only
  spawnOnce "ssh-add"
  makeWorkspaceGroup "home"
  makeWorkspaceGroup "courtroom"
  makeWorkspaceGroup "cyclist"
  makeWorkspaceGroup "healthtalker"
  makeWorkspaceGroup "espiritu"
  makeWorkspaceGroup "openrounds"

-- https://mail.haskell.org/pipermail/xmonad/2014-May/014114.html
makeWorkspaceGroup name = do
  -- addRawWSGroup name [(S 0, "0_" ++ name), (S 1, "1_" ++ name), (S 2, "2_home")]
  addRawWSGroup name [(S 0, "0_" ++ name), (S 1, "1_" ++ name), (S 2, "2_" ++ name)]

------------------------------------------------------------------------

myLogHook bar = do
  dynamicLogWithPP $ myPP { ppOutput = hPutStrLn bar }
  where
    myPP = defaultPP {
      -- do not show hidden workspaces
        ppHidden = const ""
      , ppSep    = " | "
      , ppSort   = getSortByXineramaRule
      -- do not show window title
      , ppOrder  = \(ws:l:t:_) -> [ws,l]
      , ppCurrent = xmobarColor "green" "" . wrap "(" ")"
      , ppVisible = wrap "(" ")"
      , ppUrgent  = xmobarColor "red" "yellow"
      }

------------------------------------------------------------------------
-- looks interesting
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-PhysicalScreens.html

myKeys conf@XConfig{modMask=modm} = M.fromList $
  [
    ((modm,               xK_f), warpToWindow (1%2) (1%2))                    -- move mouse to focused window
  , ((modm,               xK_g), promptedTopic)
  , ((modm .|. shiftMask, xK_g), currentTopicAction myTopicConfig)
  , ((modm,               xK_i), sendMessage $ Toggle REFLECTX)               -- reflex layout horizontally
  , ((modm .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore)) -- toggle maximize
  , ((modm .|. shiftMask, xK_p), myScreenshotPrompt defaultXPConfig)          -- screenshot
  , ((modm,               xK_x), spawn "sleep 1; xset dpms force off")
  -- run menu 
  --, ((modm .|. shiftMask    , xK_), spawnSelected defaultGSConfig ["gnome-system-monitor", "xfe", "pcmanfm"])
  , ((modm,               xK_b), submap . M.fromList $
      let brightness = \(k, b) -> ((0, k), spawn ("/home/chris/bin/brightness/brightness2 set " ++ show b))
      in map brightness (zip ([xK_1..xK_9] ++ [xK_0]) [10,20..])
    )
  ]
  ++
  [
    ((modm .|. shiftMask, xK_Return), spawnShell)                         -- %! Launch terminal
  , ((modm,               xK_p     ), spawn "gmrun")                      -- %! Launch gmrun
  --, ((modm .|. shiftMask, xK_p     ), spawn "dmenu_run")                  -- %! Launch dmenu
  , ((modm .|. shiftMask, xK_c     ), kill)                               -- %! Close the focused window
  , ((modm,               xK_space ), sendMessage NextLayout)             -- %! Rotate through the available layout algorithms
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
  , ((modm,               xK_n     ), refresh)                            -- %! Resize viewed windows to the correct size
  , ((modm,               xK_Tab   ), windows W.focusDown)                -- %! Move focus to the next window
  , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  )                -- %! Move focus to the previous window
  , ((modm,               xK_j     ), windows W.focusDown)                -- %! Move focus to the next window
  , ((modm,               xK_k     ), windows W.focusUp  )                -- %! Move focus to the previous window
  , ((modm,               xK_m     ), windows W.focusMaster  )            -- %! Move focus to the master window
  , ((modm,               xK_Return), windows W.swapMaster)               -- %! Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )               -- %! Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )               -- %! Swap the focused window with the previous window
  , ((modm,               xK_h     ), sendMessage Shrink)                 -- %! Shrink the master area
  , ((modm,               xK_l     ), sendMessage Expand)                 -- %! Expand the master area
  , ((modm,               xK_t     ), withFocused $ windows . W.sink)     -- %! Push window back into tiling
  , ((modm              , xK_comma ), sendMessage (IncMasterN 1))         -- %! Increment the number of windows in the master area
  , ((modm              , xK_period), sendMessage (IncMasterN (-1)))      -- %! Deincrement the number of windows in the master area
  , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))          -- %! Quit xmonad
  , ((modm              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  ]
  ++
  -- mod-{w,e}, Switch to physical/Xinerama screens 1, 2, 3
  -- mod-shift-{w,e}, Move client to screen 1, 2, 3
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_e, xK_r, xK_w] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modm, k), windows $ onCurrentScreen f i)
     | (i, k) <- zip (workspaces' conf) ([xK_1 .. xK_9] ++ [xK_0])
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------

main :: IO ()
main = do
  -- spawn "xmobar --screen 0 ~/.xmobarrc1"
  -- spawn "xmobar --screen 1 ~/.xmobarrc2"
  -- bar <- spawnPipe "xmobar --screen 2 ~/.xmobarrc3"
  bar <- spawnPipe "xmobar ~/.xmobarrc"
  nScreens <- countScreens
  -- checkTopicConfig myTopicNames myTopicConfig
  xmonad $ fullscreenSupport $ docks defaults {
    logHook    = myLogHook bar
  , manageHook = manageDocks <+> myManageHook
  }

------------------------------------------------------------------------
-- https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs

defaults = defaultConfig {
  focusFollowsMouse  = True,
  modMask            = myModMask,
  terminal           = myTerminal,
  workspaces         = myTopicNames,   -- withScreens nScreens myTopicNames
  -- borders
  borderWidth        = 4,
  normalBorderColor  = "#111",
  focusedBorderColor = "#24A2FF",
  -- key bindings
  keys               = myKeys,
  --mouseBindings      = myMouseBindings,
  -- hooks, layouts
  layoutHook         = myLayouts,
  manageHook         = myManageHook,
  startupHook        = myStartupHook
}

