{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Main where

import XMonad
import qualified XMonad.StackSet as StackSet
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Fullscreen

import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedActions (addDescrKeys')
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook, namedScratchpadFilterOutWorkspacePP)


import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Actions.DynamicProjects (dynamicProjects)
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder, getWsCompareByOrder)

import System.IO
import Data.Monoid
import Data.List (elemIndex, sortBy)

import KeyBinds
import Actions
import Styles
import Layout
import Workspaces

myFocusFollowsMouse = True
myClickJustFocuses = False

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"

    xmonad $
      fullscreenSupport $
      dynamicProjects projects $
      withNavigation2DConfig myNav2DConf $
      ewmh $
      addDescrKeys' ((myModMask, xK_F1), showKeybindings) KeyBinds.myKeys $
      myConfig xmproc


myConfig p = def
  { borderWidth        = myBorderWidth
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myHighlightTextColor

  , clickJustFocuses   = myClickJustFocuses
  , focusFollowsMouse  = myFocusFollowsMouse

  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , layoutHook         = myLayoutHook
  , logHook            = myLogHook p
  , startupHook        = myStartupHook

  , mouseBindings      = myMouseBindings
  , modMask            = myModMask
  , terminal           = myTerminal
  , workspaces         = myWorkspaces
  }


myManageHook :: ManageHook
myManageHook = do
  manageDocks
  manageSpawn
  namedScratchpadManageHook scratchpads
  composeOne [
      isScratchpadTerminal -?> doCenterFloat
    , isSpotify -?> doCenterFloat
    , isCalculator -?> doCenterFloat
    ]


myHandleEventHook :: Event -> X All
myHandleEventHook = docksEventHook


myLogHook :: Handle -> X ()
myLogHook xmproc = do
  ewmhDesktopsLogHook

  dynWorkspaces <- gets $ filter (/= "NSP") . map StackSet.tag . StackSet.workspaces . windowset
  sortedWs <- (`sortBy` dynWorkspaces) <$> getWsCompareByOrder

  dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP xmobarPP {
        ppOutput = hPutStrLn xmproc
      , ppCurrent =  xmobarColor xmobarCurrentWorkspaceColor "" . addWSNumber sortedWs
      , ppVisible = makeClickable sortedWs
      , ppHidden = xmobarColor xmobarHiddenWorkspaceColor "" . makeClickable sortedWs
      , ppHiddenNoWindows = xmobarColor xmobarHiddenWorkspaceColor "" . makeClickable sortedWs
      , ppTitle = hide
      , ppSort = getSortByOrder
      , ppSep = "   "
      , ppWsSep = "  "
      , ppLayout = hide
  }

  where
    hide = const ""

    makeClickable sortedWs wId = case elemIndex wId sortedWs of
      Nothing -> wId
      Just n -> "<action=xdotool key super+" ++ show (n+1) ++ ">" ++ addWSNumber sortedWs wId ++ "</action>"

    addWSNumber sortedWs wId = case elemIndex wId sortedWs of
      Nothing -> wId
      Just n -> "<fn=2>" ++ show (n+1) ++ "</fn> " ++  wId

myStartupHook :: X ()
myStartupHook = do
  spawn "xmodmap ~/.Xmodmap"
  spawn "compton --backend glx --config ~/compton.conf"
  setWMName "LG3D"
  docksStartupHook
