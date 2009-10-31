{- xmonad.hs
 - Author: brenix (modified) original source: 'Mr.Elendig'
 - Version: 0.0.9
 -}

-------------------------------------------------------------------------------
-- Imports --
-- stuff
import Text.Regex.Posix((=~))
import Data.List(isInfixOf)
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
import XMonad.Layout.DecorationMadness
--import XMonad.Layout.Accordion
import Data.Ratio((%))

-- prompts
import XMonad.Prompt
import XMonad.Prompt.Theme
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell

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
    , className =? "Pidgin" --> doF (W.shift "im") 
    , className =? "gmrun"  --> doFloat
    , className =? "Swiftfox" --> doF (W.shift "web")
    , className =? "MPlayer" --> doF (W.shift "media")
    , className =? "Smplayer" --> doF (W.shift "media") <+> doF(W.swapUp)
    , className =? "Gvim" --> doF (W.shift "dev") <+> doF(W.swapUp)
    , className =? "Evince" --> doF (W.shift "reading") <+> doF(W.swapUp)
    , className =? "Acroread" --> doF (W.shift "reading") <+> doF(W.swapUp)
    , className ==? "uzbl" --> doF (W.shift "web")
    , className =? "IRSSI" --> doF (W.shift "irc")
    , resource  =? "desktop_window" --> doIgnore
    ]
manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageHook defaultConfig <+> manageDocks <+> myManageHook

logHook' :: Handle ->  X ()
logHook' h = (dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }) >> fadeInactiveLogHook fadeAmount

--fadeAmount :: Rational
--fadeAmount :: Integer
--fadeAmount = 0x55555555
fadeAmount = 0.4

layoutHook' = customLayout

-- Da xmobar ne prikazuje nazive programa 
myFunc :: String -> String
myFunc _ = ""

-- Regex match of className - Jurica
q ==? x = fmap ( =~ x ) q 

-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#0b8bff" ""
                     --, ppTitle =  shorten 20
                     , ppTitle = myFunc
                     , ppSep =  "<fc=#AFAF87>  |  </fc>"
                     , ppHiddenNoWindows = xmobarColor "#ececec" ""
                     , ppUrgent = xmobarColor "#FF0000" "" . wrap "[" "]"
                     }

-- borders
borderWidth' :: Dimension
borderWidth' = 0

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#333333"
focusedBorderColor' = "#0775a8"

-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["term", "dev", "web", "media", "irc", "im", "reading", "vm", "mail"]

-- layouts
customLayout = avoidStruts 
                $ onWorkspaces ["dev", "web"] (noBorders (tabbed shrinkText (theme donaldTheme)) ||| smartBorders threeCols)
                $ onWorkspace "term" (smartBorders threeCols ||| noBorders Full)
                $ onWorkspace "im" Grid
                $ onWorkspace "reading" (noBorders Full ||| smartBorders (tabbed shrinkText (theme wfarrTheme)))
                $ onWorkspace "media" (noBorders Circle ||| smartBorders threeCols ||| noBorders Full)
                $ onWorkspace "vm" (noBorders Full)
                $ smartBorders tiled 
                ||| smartBorders (Mirror tiled)  
                ||| noBorders Full 
                ||| noBorders (tabbed shrinkText (theme smallClean))
                ||| Grid
                ||| smartBorders Circle
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

openInUzblXPC :: XPConfig
openInUzblXPC = defaultXPConfig
    { defaultText       = "uzbl-browser --uri " 
    , font              = "xft:Monaco 9"
    , height            = 28
    , bgColor           = "black"
    , fgColor           = "#684"
    , bgHLight          = "#785"
    , fgHLight          = "black"
    , promptBorderWidth = 0
    , position          = Top
    }

myDarkXPC :: XPConfig
myDarkXPC = defaultXPConfig
    { font = "xft:Monaco 9"
    , height            = 28
    , bgColor           = "black"
    , fgColor           = "#684"
    , bgHLight          = "#785"
    , fgHLight          = "black"
    , promptBorderWidth = 0
    , position          = Top
    }

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((modMask' .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((modMask', xK_Return), (windows $ W.greedyView "term") >> spawn "terminator")
        , ((modMask', xK_f), (windows $ W.greedyView "web") >> runOrRaise "swiftfox" (className =? "Swiftfox"))
        , ((modMask', xK_p), (windows $ W.greedyView "im") >> spawn "pidgin")
        , ((modMask', xK_a), (windows $ W.greedyView "media") >> spawn "sonata")
        , ((modMask', xK_g), (windows $ W.greedyView "dev") >> spawn "gvim")
        --, ((modMask', xK_m), (windows $ W.greedyView "media") >> spawn "smplayer")
        , ((modMask', xK_i), (windows $ W.greedyView "irc") >> spawn "terminator --title=IRSSI -e irssi")
        , ((modMask', xK_Left), spawn "mpc prev")
        , ((modMask', xK_Right), spawn "mpc next")
        , ((modMask', xK_Up), spawn "mpc toggle")
        , ((mod1Mask, xK_F2), spawn "gmrun")
        , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 1-")
        , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 1+")
        , ((0, xF86XK_AudioMute), spawn "amixer -q sset Master toggle")
        , ((0, xF86XK_ScreenSaver), spawn "xscreensaver-command -lock")
        , ((0, xF86XK_Sleep), spawn "sudo pm-suspend")
        , ((modMask' .|. controlMask, xK_t), themePrompt myDarkXPC)
        , ((modMask', xK_n), appendFilePrompt myDarkXPC "/home/m00nblade/NOTES")
        , ((modMask' .|. shiftMask, xK_x), sendMessage ToggleStruts)
        , ((modMask', xK_u), (windows $ W.greedyView "web") >> spawn "uzbl-browser")
        , ((modMask', xK_o), spawn "/home/m00nblade/.bin/open_browser.sh")
        ]

