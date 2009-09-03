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
import XMonad.Hooks.UrgencyHook

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
       xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ defaultConfig 
              { workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , logHook = logHook' h 
              , layoutHook = layoutHook'
              , manageHook = manageHook'
              } `additionalKeys` myKeys


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
                     , ppTitle =  shorten 20
                     , ppSep =  "<fc=#AFAF87>  |  </fc>"
                     , ppHiddenNoWindows = xmobarColor "#ececec" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     }

-- borders
borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#333333"
focusedBorderColor' = "#0775a8"

-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["term", "dev", "web", "media", "irc", "im", "reading", "8", "9"]

-- layouts
customLayout = avoidStruts 
                $ onWorkspaces ["dev", "web"] (smartBorders (tabbed shrinkText (theme wfarrTheme)) ||| smartBorders Full ||| smartBorders threeCols )
                $ onWorkspace "term" (smartBorders threeCols ||| noBorders Full)
                $ onWorkspace "im" Grid
                $ onWorkspace "reading" (noBorders Full ||| smartBorders (tabbed shrinkText (theme wfarrTheme)))
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

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((modMask' .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
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
