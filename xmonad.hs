{- xmonad.hs
 - Author: brenix (modified) original source: 'Mr.Elendig'
 - Version: 0.0.9
 -}

-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn) 

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedWindows
import XMonad.Util.Themes
import XMonad.Util.Dmenu

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
--import XMonad.Layout.Accordion
import Data.Ratio((%))

-- prompts
import XMonad.Prompt
import XMonad.Prompt.Theme

-- actions
import XMonad.Actions.WindowGo
-- extra keys
import Graphics.X11.ExtraTypes.XF86

-------------------------------------------------------------------------------
-- Main --
main = do
       h <- spawnPipe "xmobar" 
       xmonad $ defaultConfig 
              { workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , logHook = logHook' h 
              , layoutHook = layoutHook'
              , manageHook = manageHook'
              } `additionalKeys`
              [ ((modMask' .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
              , ((modMask', xK_Return), (windows $ W.greedyView "term") >> spawn "terminator")
--              , ((mod4Mask, xK_f), (windows $ W.greedyView "web") >> spawn "swiftfox")         
              , ((modMask', xK_f), (windows $ W.greedyView "web") >> runOrRaise "swiftfox" (className =? "Swiftfox"))
              , ((modMask', xK_p), (windows $ W.greedyView "im") >> spawn "pidgin")
              , ((modMask', xK_a), (windows $ W.greedyView "media") >> spawn "sonata")
              , ((modMask', xK_g), (windows $ W.greedyView "dev") >> spawn "gvim")
              , ((modMask', xK_m), (windows $ W.greedyView "media") >> spawn "smplayer")
              , ((modMask', xK_i), (windows $ W.greedyView "irc") >> spawn "terminator --title=IRSSI -e irssi")
              , ((modMask', xK_Left), spawn "mpc prev")
              , ((modMask', xK_Right), spawn "mpc next")
              , ((modMask', xK_Up), spawn "mpc toggle")
              , ((mod1Mask, xK_F2), spawn "gmrun")
              , ((modMask', xK_r), spawn "exe=`/home/m00nblade/.scripts/dmenu_items | dmenu` && eval \"exec $exe\"") -- Launch dmenu
              , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 1-")
              , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 1+")
              , ((0, xF86XK_AudioMute), spawn "amixer -q sset Master toggle")
              , ((0, xF86XK_ScreenSaver), spawn "xscreensaver-command -lock")
              , ((0, xF86XK_Sleep), spawn "sudo pm-suspend")
              , ((mod4Mask .|. controlMask, xK_t), themePrompt defaultXPConfig)
              ]

-------------------------------------------------------------------------------
-- Hooks --
myManageHook = composeAll
    [ className =? "Gimp"   --> doFloat
    , className =? "Pidgin" --> doF (W.greedyView "im") 
    , className =? "gmrun"  --> doFloat
    , className =? "Swiftfox" --> doF (W.shift "web")
    , className =? "MPlayer" --> doF (W.shift "media")
    , className =? "Smplayer" --> doF (W.shift "media") <+> doF(W.swapUp)
    , className =? "Gvim" --> doF (W.shift "dev") <+> doF(W.swapUp)
    , className =? "Evince" --> doF (W.shift "reading") <+> doF(W.swapUp)
    , resource  =? "desktop_window" --> doIgnore
    ]
manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageHook defaultConfig <+> manageDocks <+> myManageHook

logHook' :: Handle ->  X ()
logHook' h = (dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }) >> fadeInactiveLogHook fadeAmount

fadeAmount :: Rational
--fadeAmount = 0x55555555
fadeAmount = 0.4

layoutHook' = customLayout

-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#0b8bff" ""
                     , ppTitle =  shorten 10
                     , ppSep =  "<fc=#AFAF87>  |  </fc>"
                     , ppHiddenNoWindows = xmobarColor "#ececec" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     }

-- borders
borderWidth' :: Dimension
borderWidth' = 0

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#333333"
focusedBorderColor' = "#0775a8"

-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["term", "dev", "web", "media", "irc", "im", "reading", "8", "9"]

-- layouts
customLayout = avoidStruts 
                $ onWorkspaces ["dev", "web"] (tabbed shrinkText (theme wfarrTheme)) 
                $ onWorkspace "term" (threeCols ||| noBorders Full)
                $ onWorkspace "im" Grid
                $ onWorkspace "reading" (noBorders Full ||| tabbed shrinkText (theme wfarrTheme))
                $ smartBorders tiled 
                ||| smartBorders (Mirror tiled)  
                ||| noBorders Full 
                ||| noBorders (tabbed shrinkText (theme smallClean))
                ||| Grid
    where
    tiled = ResizableTall 1 (2/100) (1/2) []
    threeCols = ThreeCol 1 (3/100) (1/2)

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "terminator"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
modMask' = mod4Mask

{-
-- keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask,               xK_p     ), spawn "pidgin") 
    , ((modMask, xK_f        ), spawn "swiftfox")
    , ((modMask, xK_r     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- mpd controls
    , ((modMask .|. controlMask,  xK_h     ), spawn "mpc prev")
    , ((modMask .|. controlMask,  xK_t     ), spawn "mpc pause")
    , ((modMask .|. controlMask,  xK_n     ), spawn "mpc play")
    , ((modMask .|. controlMask,  xK_s     ), spawn "mpc next")
    , ((modMask .|. controlMask,  xK_g     ), spawn "mpc seek -2%")
    , ((modMask .|. controlMask,  xK_c     ), spawn "mpc volume -4")
    , ((modMask .|. controlMask,  xK_r     ), spawn "mpc volume +4")
    , ((modMask .|. controlMask,  xK_l     ), spawn "mpc seek +2%")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-}

