module KeyBinds where

import XMonad
import XMonad.Operations (windows)
import qualified XMonad.StackSet as W

import XMonad.Util.NamedActions
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Actions.Navigation2D (Direction2D(D, U, L, R), windowGo, windowSwap)
import XMonad.Actions.Promote (promote)
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.FloatSnap (ifClick, snapMagicMove, snapMagicResize)

import XMonad.Layout.Hidden (hideWindow, popNewestHiddenWindow)
import XMonad.Layout.SubLayouts (GroupMsg(MergeAll, UnMerge), toSubl, onGroup, pullGroup)
import XMonad.Layout.MultiToggle (Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL))
import qualified Data.Map as M 

import Styles
import Actions
import Data.List (find)

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ displayKeybindings x


myModMask = mod4Mask

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = systemKeys ^++^ actionKeys ^++^ launcherKeys ^++^ windowKeys ^++^ layoutKeys
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
      , ("M-S-q"   , addName "Quit XMonad" $ confirmPrompt hotPromptTheme "Quit XMonad" exit)
      , ("M-x"     , addName "Lock screen" $ lockScreen)
      ]
    
    actionKeys = subKeys "Actions"
      [ ]

    launcherKeys = subKeys "Launchers"
      [ ("M-<Space>", addName "Launcher" $ spawn myLauncher)
      , ("M-S-<Space>", addName "DRUN Launcher" $ spawn myDrunLauncher)
      , ("M-<Return>", addName "Terminal" $ spawn myTerminal)
      , ("M-\\", addName "Browser" $ spawn myBrowser)
      ]

    windowKeys = subKeys "Windows" (
      [ ("M-<Backspace>", addName "Kill" kill1)
      , ("M-S-<Backspace>", addName "Kill all" $ confirmPrompt hotPromptTheme "kill all" $ killAll)

      , ("M-p", addName "Hide window to stack" $ withFocused hideWindow)
      , ("M-S-p", addName "Pop window from hidden stack" $ popNewestHiddenWindow)

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
      , ("M-S-<Tab>", addName "Swap next window" $ windows W.swapUp )
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
      
      , ("M-`", addName "Cycle layouts " $ sendMessage NextLayout)
      , ("M-S-`", addName "Cycle sublayouts " $ toSubl NextLayout)
      
      , ("M-,", addName "Decrease master windows" $ sendMessage (IncMasterN (-1)))
      , ("M-.", addName "Increase master windows" $ sendMessage (IncMasterN 1))
      ]

-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ ((myModMask,               button1) ,(\w -> focus w
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