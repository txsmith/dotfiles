{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Layout where

import XMonad

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.WindowNavigation (Direction2D(U, D, L, R), windowNavigation)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed (addTabs)
import XMonad.Layout.TabBarDecoration (shrinkText)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.IfMax
import XMonad.Layout.BorderResize

import XMonad.Actions.Navigation2D (
    Navigation2DConfig(..)
    , centerNavigation
    , lineNavigation
    , singleWindowRect)
import XMonad.Hooks.ManageDocks (avoidStruts)

import Styles

-- Transformer to make the current window fullscreen while keeping the top bar.
-- As opposed to FULL, which hides the top bar
data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
  transform FULLBAR x k = k (avoidStruts Simplest) (const x)

myLayoutHook = borderResize 
             $ smartBorders 
             $ fullScreenToggle
             $ fullBarToggle
             $ avoidStruts
             $ windowNavigation
             $ addTopBar
             $ addTabs shrinkText myTabTheme
             $ IfMax 1 singleWindowLayout bspLayout
  where
    bspLayout = spacing gap
              $ myGaps
              $ emptyBSP

    singleWindowLayout = spacing gap 
                       $ gaps [(U,gap), (D,gap), (R,largeGap), (L,largeGap)]
                       $ Simplest
      where largeGap = 240
    
    fullBarToggle = mkToggle (single FULLBAR)
    
    fullScreenToggle = mkToggle (single FULL)
    
    addTopBar = noFrillsDeco shrinkText topBarTheme

    myGaps = gaps [(U,gap), (D,gap), (R,gap), (L,gap)]

myNav2DConf :: Navigation2DConfig 
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [ ("Full", centerNavigation) ]
    , unmappedWindowRect        = [ ("Full", singleWindowRect) ]
    }