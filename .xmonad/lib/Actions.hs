{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Actions where

import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedActions

import System.IO (hPutStr, hClose)
import System.Exit 

exit :: X ()
exit = io (exitSuccess)

displayKeybindings :: MonadIO m => [((KeyMask, KeySym), NamedAction)] -> m ()
displayKeybindings x = io $ do
  h <- spawnPipe "zenity --text-info --font=terminus"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

restart :: MonadIO m => m ()
restart = spawn "xmonad --restart"

rebuildRestart :: MonadIO m => m ()
rebuildRestart = spawn "xmonad --recompile && xmonad --restart"

lockScreen :: MonadIO m => m ()
lockScreen = spawn "xset s activate"

myTerminal = "gnome-terminal"

myBrowser = "google-chrome"

myLauncher = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"