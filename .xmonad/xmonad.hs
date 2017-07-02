{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import XMonad

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)

import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.SubLayouts (subTabbed)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Gaps (gaps)

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.NamedActions (addDescrKeys')
import XMonad.Layout.Hidden (hiddenWindows)
import System.IO
import Data.Monoid

import KeyBinds
import Actions
import Styles

myModMask = mod4Mask

myFocusFollowsMouse = True
myClickJustFocuses = False

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    spawn "nm-applet &"
    spawn "dropbox start"
    spawn "/usr/bin/numlockx"
    spawn "nitrogen --restore"
    -- spawn myTerminal

    xmonad $ 
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

        , modMask            = myModMask
        -- , mouseBindings      = myMouseBindings
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        }



myManageHook :: ManageHook
myManageHook = manageDocks <> manageHook def

myHandleEventHook :: Event -> X All
myHandleEventHook = docksEventHook <> handleEventHook def


myLayoutHook = smartBorders layout


myLogHook :: Handle -> X ()
myLogHook xmproc = do
  fadeInactiveLogHook 0.5
  dynamicLogWithPP xmobarPP {
        ppOutput  = hPutStrLn xmproc
      , ppTitle   = const "" --xmobarColor myHighlightTextColor "" . shorten 100
      , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""-- xmobarColor myHighlightTextColor "" . wrap "" ""
      , ppSep     = "   "
      , ppLayout  = const ""  --xmobarColor myNormalTextColor ""
  }

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"

layout = (lessBorders OnlyFloat
               . avoidStruts
               . spacing 15
               . gaps [(U,15), (D,15), (R,15), (L,15)])
               (Tall 1 (3/100) (1/2))
         ||| avoidStruts (defaultTiling ||| (noBorders Full))
         ||| fullscreenFull ( noBorders Full )
  where
     defaultTiling  = windowNavigation $ subTabbed $ boringWindows $ hiddenWindows $ Tall numberOnMaster resizeDelta masterPaneSize
     numberOnMaster = 1
     masterPaneSize = 1/2
     resizeDelta    = 3/100

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

screenshotSelectCommand = "gnome-screenshot -i"
screenshotWindowCommand = "gnome-screenshot -i -c"
