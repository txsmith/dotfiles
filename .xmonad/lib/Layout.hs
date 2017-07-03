{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Layout (myLayoutHook) where

import XMonad

import XMonad.Layout.NoBorders
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.SubLayouts (subTabbed)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Simplest


import XMonad.Hooks.ManageDocks

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k (avoidStruts Simplest) (\_ -> x)


myLayoutHook = smartBorders 
             $ layout
  where 
    layout = (lessBorders OnlyFloat
               . avoidStruts
               . spacing 15
               . gaps [(U,15), (D,15), (R,15), (L,15)])
               defaultTiling
         ||| avoidStruts (defaultTiling ||| (noBorders Full))
         ||| fullscreenFull ( noBorders Full )

    defaultTiling  = windowNavigation $ subTabbed $ boringWindows $ hiddenWindows $ Tall numberOnMaster resizeDelta masterPaneSize
    numberOnMaster = 1
    masterPaneSize = 1/2
    resizeDelta    = 3/100
    -- fullBarToggle = mkToggle (single FULLBAR)
    -- fullScreenToggle = mkToggle (single FULL)