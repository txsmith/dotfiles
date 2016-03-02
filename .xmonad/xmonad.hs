--module MyMod where

import XMonad

import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Util.Run(spawnPipe)
import System.IO
import Data.Monoid

modKey = mod4Mask
borderNormalColor  = "#455A64"
normalTextColor    = "#5E717A"
highlightTextColor = "#20A294"

main = do
    xmproc <- spawnPipe "xmobar /home/thomas/.xmobarrc"
    spawn "nm-applet &"
    spawn "dropbox start"
    spawn "xrandr --output VGA1 --primary --above LVDS1"
    spawn "nitrogen --restore"
    xmonad $ defaultConfig {
          modMask     = modKey
        , workspaces  = myWorkspaces
        , startupHook = setWMName "LG3D"
        , manageHook  = manageDocks <+> manageHook defaultConfig
        , layoutHook  = smartBorders layout
        , logHook = dynamicLogWithPP xmobarPP {
                ppOutput  = hPutStrLn xmproc
              , ppTitle   = xmobarColor highlightTextColor "" . shorten 100
              , ppCurrent = xmobarColor highlightTextColor "" . wrap "" ""
              , ppSep     = xmobarColor normalTextColor "" " | "
              , ppUrgent  = xmobarColor "#0000ff" ""
              , ppLayout  = xmobarColor normalTextColor ""
          }
        , borderWidth        = 3
        , normalBorderColor  = borderNormalColor
        , focusedBorderColor = highlightTextColor
        }
        `additionalKeys` myKeys


layout =     avoidStruts ((Mirror defaultTiling) ||| defaultTiling)
         ||| avoidStruts (noBorders Full)
         ||| fullscreenFull ( noBorders Full )
  where
     defaultTiling  = Tall numberOnMaster resizeDelta masterPaneSize
     numberOnMaster = 1
     masterPaneSize = 1/2
     resizeDelta    = 3/100

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

screenshotSelectCommand = "~/.xmonad/screenshot-select.sh"
screenshotWindowCommand = "~/.xmonad/screenshot-window.sh"

myKeys = [
        ((modKey, xK_Print),               spawn screenshotSelectCommand)
      , ((modKey .|. shiftMask, xK_Print), spawn screenshotWindowCommand)
      , ((0, 0x1008FF11),    spawn "amixer set Master 3-")
      , ((0, 0x1008FF13),    spawn "amixer set Master 3+")
      , ((0, 0x1008FF12),    spawn "amixer set Master toggle")
    ] ++
    [((m .|. modKey, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
