{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Main where

import XMonad
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
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)

import System.IO
import Data.Monoid
import Data.List (elemIndex, isPrefixOf)
import Data.Char (toLower)

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



isSpotify = do
  c <- stringProperty "WM_NAME"
  trace $ "CLASS IS:" ++ c
  return $ ("spotify" `isPrefixOf`) $ (toLower <$> c)


myManageHook :: ManageHook
myManageHook = do
  manageDocks
  manageSpawn
  namedScratchpadManageHook scratchpads
  composeOne [ 
      isScratchpadTerminal -?> doCenterFloat
    , isCalculator -?> doCenterFloat
    ] 


myHandleEventHook :: Event -> X All
myHandleEventHook = do 
  docksEventHook


myLogHook :: Handle -> X ()
myLogHook xmproc = do
  ewmhDesktopsLogHook
  dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP xmobarPP {
        ppOutput = hPutStrLn xmproc
      , ppCurrent =  xmobarColor xmobarCurrentWorkspaceColor ""
      , ppVisible = makeClickable
      , ppHidden = xmobarColor xmobarHiddenWorkspaceColor "" . makeClickable
      , ppHiddenNoWindows = xmobarColor xmobarHiddenWorkspaceColor "" . makeClickable
      , ppTitle = hide
      , ppSort = getSortByOrder
      , ppSep = "   "
      , ppWsSep = "  "
      , ppLayout = hide
  }

  where 
    hide = const ""

    makeClickable wId = case elemIndex wId myWorkspaces of
      Nothing -> wId
      Just n -> "<action=xdotool key super+" ++ show (n+1) ++ ">" ++ wId ++ "</action>"


myStartupHook :: X ()
myStartupHook = do
  spawn "xmodmap ~/.Xmodmap"
  spawn "compton --backend glx --config ~/compton.conf"
  setWMName "LG3D"
  docksStartupHook
