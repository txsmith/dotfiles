module KeyBinds where

import XMonad
import XMonad.Core
import XMonad.Operations (windows, sendMessage)
import qualified XMonad.StackSet as W

import XMonad.Util.NamedActions
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspace)

import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.XMonad (xmonadPromptC)
import XMonad.Prompt.Pass

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Actions.Navigation2D (Direction2D(D, U, L, R), windowGo, windowSwap)
import XMonad.Actions.Promote (promote)
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.FloatSnap (ifClick, snapMagicMove, snapMagicResize)
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace, removeWorkspace)
import XMonad.Actions.DynamicProjects (switchProjectPrompt, shiftToProjectPrompt, renameProjectPrompt)
import XMonad.Actions.CycleWS (Direction1D(Next, Prev), WSType(..), toggleWS, findWorkspace, nextScreen, swapNextScreen)
import XMonad.Layout.Hidden (hideWindow, popNewestHiddenWindow)
import XMonad.Layout.SubLayouts (GroupMsg(MergeAll, UnMerge), toSubl, onGroup, pullGroup)
import XMonad.Layout.MultiToggle (Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL))
import XMonad.Layout.Gaps (GapMessage(IncGap, DecGap))

import Styles
import Actions
import Layout
import Data.List (find)
import qualified Data.Map as M 

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ displayKeybindings x

myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = systemKeys ^++^ launcherKeys 
            ^++^ windowKeys ^++^ layoutKeys ^++^ workspaceKeys
  where

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    -- screenKeys     = ["w","v","z"]
    arrowKeys      = ["<U>","<D>","<L>","<R>"]
    dirKeys        = ["w", "s", "a", "d"]
    dirs           = [ U, D, L, R ]

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

    systemKeys = subKeys "System"
      [ ("M-q"     , addName "Restart XMonad" Actions.restart)
      , ("M-C-q"   , addName "Rebuild & restart XMonad" rebuildRestart)
      , ("M-S-q"   , addName "Quit XMonad" $ xmonadPromptC [
                      ("Lock", lockScreen)
                    , ("Shutdown", shutdown)
                    , ("Logout", exit)
                    , ("Reboot", reboot)
                    ] myPromptTheme)
      ]
    
    launcherKeys = subKeys "Launchers"
      [ ("M-<Space>", addName "Launcher" $ spawn myLauncher)
      , ("M-S-<Space>", addName "DRUN Launcher" $ spawn myDrunLauncher)
      , ("M-<Return>", addName "Terminal" $ spawn myTerminal)
      , ("M-\\", addName "Browser" $ spawn myBrowser)
      , ("M-<Print>", addName "Screenshot" $ spawn screenshotSelectCommand)
      , ("M-o", addName "Password lookup dialog" $ passPrompt myPromptTheme)
      ]

    windowKeys = subKeys "Windows" (
      [ ("M-<Backspace>", addName "Kill" kill1)
      , ("M-S-<Backspace>", addName "Kill all" $ confirmPrompt hotPromptTheme "kill all" killAll)

      , ("M-h", addName "Hide window to stack" $ withFocused hideWindow)
      , ("M-S-h", addName "Pop window from hidden stack" $ popNewestHiddenWindow)

      , ("M-g", addName "Un-merge from sublayout" $ withFocused (sendMessage . UnMerge))
      , ("M-S-g", addName "Merge all into sublayout" $ withFocused (sendMessage . MergeAll))

      , ("M-m", addName "Promote to master" $ promote)

      -- Tabs
      , ("M-'", addName "Navigate previous tab" $ bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
      , ("M-;", addName "Navigate next tabs" $ bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])
      , ("C-'", addName "Swap tab with previous" $ windows W.swapDown)
      , ("C-;", addName "Swap tab with next" $ windows W.swapUp)
      
      -- Windows
      , ("M-<Tab>", addName "Navigate next window" $ windows W.focusUp )
      , ("M-S-<Tcab>", addName "Swap next window" $ windows W.swapUp )
      ] 
        -- Move / Navigate thourgh windows with M-<w,s,a,d>
        ++ zipM' "M-" "Navigate window" dirKeys dirs windowGo True
        ++ zipM' "M-S-" "Move window" dirKeys dirs windowSwap True
        ++ zipM  "M-C-" "Merge w/sublayout" dirKeys dirs (sendMessage . pullGroup)

        -- Move / Navigate thourgh windows with M-<left, up, down, right>
        ++ zipM' "M-" "Navigate window" arrowKeys dirs windowGo True
        ++ zipM' "M-S-" "Move window" arrowKeys dirs windowSwap True
        ++ zipM  "M-C-" "Merge w/sublayout" arrowKeys dirs (sendMessage . pullGroup)
      )

    layoutKeys = subKeys "Layout Management"
      [ ("M-y", addName "Float tiled w" $ withFocused toggleFloat)
      , ("M-S-y", addName "Tile all floating w" sinkAll)
      
      , ("M-f", addName "Fullscreen" $ sequence_ [ (withFocused $ windows . W.sink)
                                                 , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])
      , ("M-S-f", addName "Fullscreen" $ sequence_ [ (withFocused $ windows . W.sink)
                                                 , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULLBAR) ])

      , ("M-`", addName "Cycle layouts " $ sendMessage NextLayout)
      , ("M-S-`", addName "Cycle sublayouts " $ toSubl NextLayout)
      
      , ("M-,", addName "Decrease master windows" $ sendMessage (IncMasterN (-1)))
      , ("M-.", addName "Increase master windows" $ sendMessage (IncMasterN 1))
      ]

    workspaceKeys = subKeys "Workspaces & Projects" (
      [ ("M-p", addName "Switch to Project" $ switchProjectPrompt myPromptTheme)
      , ("M-S-p", addName "Shift to Project" $ shiftToProjectPrompt myPromptTheme)
      , ("M-C-p", addName "Rename Project" $ renameProjectPrompt myPromptTheme)
      , ("M-C-<Backspace>", addName "Remove Project" $ confirmPrompt hotPromptTheme "Remove Workspace?" $ removeWorkspace)
      , ("M-l", addName "Toggle last workspace" toggleWS)
      , ("M-=", addName "Next non-empty workspace" nextNonEmptyWS)
      , ("M--", addName "Prev non-empty workspace" prevNonEmptyWS)
      , ("M-k", addName "Next non-empty workspace" nextNonEmptyWS)
      , ("M-j", addName "Prev non-empty workspace" prevNonEmptyWS)
      , ("M-e", addName "Next visible workspace" $ nextScreen)
      , ("M-S-e", addName "Swap with next visible workspace" $ swapNextScreen)
      ]
        ++ zipM "M-" "View      ws" wsKeys [0..] (withNthWorkspace W.view)
        ++ zipM "M-S-" "Move w to ws" wsKeys [0..] (withNthWorkspace W.shift)
      )
    
    wsKeys = show <$> [1..9] ++ [0]

    nextNonEmptyWS = do
      t <- findWorkspace getSortByIndexNoSP Next HiddenWS 1
      windows $ W.view t

    prevNonEmptyWS = do
      t <- findWorkspace getSortByIndexNoSP Prev HiddenWS 1
      windows $ W.view t

    getSortByIndexNoSP =
            fmap (. namedScratchpadFilterOutWorkspace) getSortByIndex
            


-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ 
      ((myModMask,               button1) ,(\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button1), (\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask,               button3), (\w -> focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button3), (\w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster ))

    -- Scroll up
    , ((myModMask, button4), (\w -> do
        trace "Decrease gaps"
        sendMessage $ DecGap 48 L
        sendMessage $ DecGap 48 R
      ))
    -- Scroll down
    , ((myModMask, button5), (\w -> do
        trace "Increase gaps"
        sendMessage $ IncGap 48 L
        sendMessage $ IncGap 48 R
      ))
    ]



data XCond = WS | LD

-- | Choose an action based on the current workspace id (WS) or
-- layout description (LD).
chooseAction :: XCond -> (String->X()) -> X()
chooseAction WS f = withWindowSet (f . W.currentTag)
chooseAction LD f = withWindowSet (f . description . W.layout . W.workspace . W.current)

-- | If current workspace or layout string is listed, run the associated
-- action (only the first match counts!) If it isn't listed, then run the default
-- action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: XCond -> [(String, X())] -> X()
bindOn xc bindings = chooseAction xc $ chooser where
    chooser xc = case find ((xc==).fst) bindings of
        Just (_, action) -> action
        Nothing -> case find ((""==).fst) bindings of
            Just (_, action) -> action
            Nothing -> return ()
