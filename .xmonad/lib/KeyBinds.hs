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
import XMonad.Layout.Hidden (hideWindow, popNewestHiddenWindow)
import XMonad.Layout.SubLayouts (GroupMsg(MergeAll, UnMerge), toSubl)

import qualified Data.Map as M 

import Styles
import Actions


-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ displayKeybindings x


myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = systemKeys ^++^ actionKeys ^++^ launcherKeys ^++^ windowKeys ^++^ layoutKeys
  where

    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    -- screenKeys     = ["w","v","z"]
    dirKeys        = ["w", "s", "a", "d"]
    arrowKeys      = ["<U>","<D>","<L>","<R>"]
    dirs           = [ U, D, L, R ]

    --screenAction f        = screenWorkspace >=> flip whenJust (windows . f)

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    -- from xmonad.layout.sublayouts
    -- focusMaster' st = let (f:fs) = W.integrate st
    --     in W.Stack f [] fs
    -- swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

    -- -- try sending one message, fallback if unreceived, then refresh
    -- tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]

    -- -- warpCursor = warpToWindow (9/10) (9/10)

    -- -- cf https://github.com/pjones/xmonadrc
    -- --switch :: ProjectTable -> ProjectName -> X ()
    -- --switch ps name = case Map.lookup name ps of
    -- --  Just p              -> switchProject p
    -- --  Nothing | null name -> return ()

    -- -- do something with current X selection
    -- unsafeWithSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

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
      , ("M-<Return>", addName "Terminal" $ spawn myTerminal)
      , ("M-\\", addName "Browser" $ spawn myBrowser)
      ]

    windowKeys = subKeys "Windows" (
      [ ("M-<Backspace>", addName "Kill" kill1)
      , ("M-S-<Backspace>", addName "Kill all" $ confirmPrompt hotPromptTheme "kill all" $ killAll)
      , ("M-p", addName "Hide window to stack" $ withFocused hideWindow)
      , ("M-<Tab>-p", addName "Pop window from hidden stack" $ popNewestHiddenWindow)
      , ("M-g", addName "Un-merge from sublayout" $ withFocused (sendMessage . UnMerge))
      , ("M-S-g", addName "Merge all into sublayout" $ withFocused (sendMessage . MergeAll))
      , ("M-m", addName "Focus master" $ windows W.focusMaster)
      -- , ("M-'", addName "Navigate tabs D" $ bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')])
      -- , ("M-;", addName "Navigate tabs U" $ bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')])
      , ("C-'", addName "Swap tab D" $ windows W.swapDown)
      , ("C-;", addName "Swap tab U" $ windows W.swapUp)
      ] ++ zipM' "M-" "Navigate window" dirKeys dirs windowGo True
        ++ zipM' "M-S-" "Move window" dirKeys dirs windowSwap True

      )

    layoutKeys = subKeys "Layout Management"
      [ ("M-<Tab>"                , addName "Cycle all layouts"               $ sendMessage NextLayout)
      , ("M-C-<Tab>"              , addName "Cycle sublayout"                 $ toSubl NextLayout)
      , ("M-S-<Tab>"              , addName "Reset layout"                    $ setLayout $ XMonad.layoutHook conf)

      , ("M-y"                    , addName "Float tiled w"                   $ withFocused toggleFloat)
      , ("M-S-y"                  , addName "Tile all floating w"             $ sinkAll)

      , ("M-,"                    , addName "Decrease master windows"         $ sendMessage (IncMasterN (-1)))
      , ("M-."                    , addName "Increase master windows"         $ sendMessage (IncMasterN 1))
      ]