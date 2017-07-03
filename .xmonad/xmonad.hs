{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Main where

import XMonad

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place (placeHook, fixed)

import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedActions (addDescrKeys')

import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Layout.Fullscreen (fullscreenManageHook)

import System.IO
import Data.Monoid
import Data.List (isPrefixOf)
import Data.Char (toLower)

import KeyBinds
import Actions
import Styles
import Layout

myFocusFollowsMouse = True
myClickJustFocuses = False

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    -- spawn "nm-applet &"

    xmonad $ 
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
  fullscreenManageHook
  manageSpawn
  composeAll [ 
      (isLauncher <||> isSpotify) --> ((placeHook $ fixed (1, 0))) -- 
    ] 
  -- manageHook def

myHandleEventHook :: Event -> X All
myHandleEventHook = do 
  docksEventHook
  handleEventHook def


myLogHook :: Handle -> X ()
myLogHook xmproc = do
  ewmhDesktopsLogHook
  dynamicLogWithPP xmobarPP {
        ppOutput  = hPutStrLn xmproc
      , ppTitle   = const ""
      , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
      , ppSep     = "   "
      , ppLayout  = const ""
  }

myStartupHook :: X ()
myStartupHook = do
  spawn "compton --backend glx --config ~/compton.conf"
  setWMName "LG3D"


myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
