{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Main where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place (placeHook, fixed)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedActions (addDescrKeys')
import XMonad.Util.WorkspaceCompare

import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Actions.DynamicProjects (dynamicProjects)

import XMonad.Layout.Fullscreen (fullscreenManageHook, fullscreenEventHook)

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

isLauncher = stringProperty "WM_NAME" =? "Terminal"

myManageHook :: ManageHook
myManageHook = do
  manageDocks
  manageSpawn
  composeAll [ 
      isFullscreen --> doFullFloat
    , (isLauncher <||> isSpotify) --> ((placeHook $ fixed (1, 0))) -- 
    ] 
  -- manageHook def

myHandleEventHook :: Event -> X All
myHandleEventHook = do 
  docksEventHook
  XMonad.Hooks.EwmhDesktops.fullscreenEventHook
  handleEventHook def


myLogHook :: Handle -> X ()
myLogHook xmproc = do
  ewmhDesktopsLogHook
  dynamicLogWithPP xmobarPP {
        ppOutput  = hPutStrLn xmproc
      , ppCurrent =  xmobarColor xmobarCurrentWorkspaceColor ""
      , ppVisible = makeClickable
      , ppHidden = xmobarColor xmobarHiddenWorkspaceColor "" . makeClickable
      , ppHiddenNoWindows = xmobarColor xmobarHiddenWorkspaceColor "" . makeClickable
      , ppTitle   = hide
      , ppSort = getSortByIndex
      , ppSep     = "   "
      , ppWsSep   = "  "
      , ppLayout  = hide
  }

  where 
    hide = const ""

    makeClickable wId = case elemIndex wId myWorkspaces of
      Nothing -> wId
      Just n -> "<action=xdotool key alt+" ++ show (n+1) ++ ">" ++ wId ++ "</action>"


myStartupHook :: X ()
myStartupHook = do
  spawn "compton --backend glx --config ~/compton.conf"
  setWMName "LG3D"
  ewmhDesktopsStartup

