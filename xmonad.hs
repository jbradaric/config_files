{- xmonad.hs
 - Author: Jurica Bradaric 
 - Version: 0.9.2
 -}

-------------------------------------------------------------------------------
-- Imports --
{- misc -}
import Data.List(isInfixOf)
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn) 

{- XMonad stuff -}
import XMonad
import qualified XMonad.StackSet as W

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Themes (theme,donaldTheme)
import XMonad.Util.Scratchpad (scratchpadSpawnAction)

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
import Data.Ratio((%))

-- prompts
import XMonad.Prompt
import XMonad.Prompt.AppendFile

-- actions
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.FocusNth (focusNth)
import qualified XMonad.Actions.Search as S

-- extra keys
import Graphics.X11.ExtraTypes.XF86

-------------------------------------------------------------------------------
-- Main --
main = do
       h <- spawnPipe "xmobar" 
       xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ myConfig
        { logHook = logHook' h } `additionalKeysP` myKeys


-------------------------------------------------------------------------------

myConfig = defaultConfig
    { workspaces = workspaces'
    , modMask = modMask'
    , borderWidth = borderWidth'
    , normalBorderColor = normalBorderColor'
    , focusedBorderColor = focusedBorderColor'
    , terminal = terminal'
    , layoutHook = layoutHook'
    , manageHook = manageHook'
    }

-- Hooks --
myManageHook = (composeAll . concat)
    [ [className =? name --> doShift wspace | (name, wspace) <- myShifts]
    , [className =? name --> doFloat | name <- myFloats]
    , [resource  =? resName --> doIgnore | resName <- myIgnored]
--    , [className =? "uzbl" --> doF (W.shift "web") <+> doF W.focusDown]
    , [className =? name --> doShift wspace <+> doF W.focusDown | (name, wspace) <- myBackgrounded]
    ]
    where myShifts = [ ("Pidgin", "im")
                     , ("Swiftfox", "web")
                     , ("Firefox", "web")
                     , ("MPlayer", "media")
                     , ("Smplayer", "media")
                     , ("Gvim", "dev")
                     , ("Evince", "reading")
                     , ("Acroread", "reading")
                     , ("IRSSI", "irc")
                     ]
          myFloats = ["Gimp"]
          myIgnored = ["desktop_window"]
          myBackgrounded = [("uzbl", "web")]

manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageHook defaultConfig <+> manageDocks <+> myManageHook

logHook' :: Handle ->  X ()
logHook' h = (dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }) >> fadeInactiveLogHook fadeAmount

fadeAmount :: Rational
fadeAmount = 0.4

layoutHook' = customLayout

-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#0b8bff" ""
                     --, ppTitle =  shorten 20
                     , ppTitle = \_ -> "" -- Don't show the window title in xmobar
                     , ppSep =  "<fc=#AFAF87>  |  </fc>"
                     --, ppHiddenNoWindows = xmobarColor "#ececec" ""
                     , ppUrgent = xmobarColor "#FF0000" "" . wrap "[" "]"
                     , ppHidden = \_ -> ""
                     , ppHiddenNoWindows = \_ -> ""
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
                $ onWorkspaces ["term", "irc"] (smartBorders threeCols ||| noBorders Full)
                $ onWorkspace "im" Grid
                $ onWorkspace "reading" (smartBorders (tabbed shrinkText (theme donaldTheme)))
                $ onWorkspace "media" (noBorders Circle ||| noBorders Full)
                $ onWorkspace "vm" (noBorders Full)
                $ onWorkspace "mail" (noBorders Full ||| smartBorders threeCols)
                $ smartBorders tiled 
    where
    tiled = ResizableTall 1 (2/100) (1/2) []
    threeCols = ThreeCol 1 (3/100) (1/2)

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvtc"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
modMask' = mod4Mask

myDarkXPC :: XPConfig
myDarkXPC = defaultXPConfig
    { font              = "xft:Monaco 9"
    , height            = 28
    , bgColor           = "black"
    , fgColor           = "#684"
    , bgHLight          = "#785"
    , fgHLight          = "black"
    , promptBorderWidth = 0
    , position          = Top
    }

myScratchpadConf = defaultConfig
    { terminal = "urxvtc"
    }

myKeys :: [(String, X ())]
myKeys = concat
        [
         [ ("M-f", (windows $ W.greedyView "web") >> runOrRaise "swiftfox" (className =? "Swiftfox"))
         , ("M-p", (windows $ W.greedyView "im") >> spawn "pidgin")
         , ("M-a", (windows $ W.greedyView "media") >> spawn "sonata")
         , ("M-i", (windows $ W.greedyView "irc") >> spawn "urxvtc -title IRSSI -e irssi")
         , ("M-<Left>", spawn "mpc prev")
         , ("M-<Right>", spawn "mpc next")
         , ("M-<Up>", spawn "mpc toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 1-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 1+")
         , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
         , ("<XF86ScreenSaver>", spawn "i3lock")
         , ("M-n", appendFilePrompt myDarkXPC "/home/m00nblade/NOTES")
         , ("M-S-x", sendMessage ToggleStruts)
         , ("M-u", (windows $ W.greedyView "web") >> spawn "uzbl-browser")
         , ("M-o", spawn "/home/m00nblade/bin/open_browser.sh")
         , ("M-S-t", scratchpadSpawnAction myScratchpadConf) ]
         , [ ("C-" ++ (show k), focusNth i) | (i, k) <- zip [0..8] [1..]] -- focus the nth window with <Ctrl>-#
         , [ ("M-s " ++ k, S.promptSearchBrowser myDarkXPC "uzbl-browser" f) | (k, f) <- searchList]
         , [ ("M-S-s " ++ k, S.selectSearchBrowser "uzbl-browser" f) | (k, f) <- searchSelectionList]
        ]

searchList :: [(String, S.SearchEngine)]
searchList = [ ("g", S.google)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("i", S.imdb)
             , ("o", myNewEngine)
             ]
            where myNewEngine = S.searchEngine "uzbl" "" -- TODO: use searchEngineF

searchSelectionList :: [(String, S.SearchEngine)]
searchSelectionList = ("t", S.thesaurus) : searchList
