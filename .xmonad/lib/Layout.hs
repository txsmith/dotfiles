{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Layout where

import XMonad

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.WindowNavigation (Direction2D(U, D, L, R), windowNavigation)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed (addTabs)
import XMonad.Layout.TabBarDecoration (shrinkText)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.IfMax

import XMonad.Actions.Navigation2D (
    Navigation2DConfig(..)
    , centerNavigation
    , lineNavigation
    , singleWindowRect)
import XMonad.Hooks.ManageDocks (avoidStruts)
import Data.Ratio ((%))

import Styles

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
  transform FULLBAR x k = k (avoidStruts Simplest) (const x)

myLayoutHook = smartBorders 
             $ fullScreenToggle
             $ fullBarToggle
             $ hiddenWindows
             $ boringWindows
             $ avoidStruts flex
  where

    flex = windowNavigation
         $ addTopBar
         $ addTabs shrinkText myTabTheme
         $ subLayout [] (Accordion ||| Simplest)
         $ IfMax 1 (singleWindowLayout ||| Simplest) ((standardLayouts $ 1%2) ||| (standardLayouts $ 2%3))
     
    singleWindowLayout = spacing gap 
                       $ gaps [(U,gap), (D,gap), (R,largeGap), (L,largeGap)]
                       $ Simplest
      where largeGap = 480

    standardLayouts r = spacing gap 
                    $ myGaps 
                    $ (ResizableTall 1 (1/20) r [])
    
    fullBarToggle = mkToggle (single FULLBAR)
    
    fullScreenToggle = mkToggle (single FULL)
    
    addTopBar = noFrillsDeco shrinkText topBarTheme

    myGaps =  gaps [(U,gap), (D,gap), (R,gap), (L,gap)]

myNav2DConf :: Navigation2DConfig 
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [ ("Full", centerNavigation) ]
    , unmappedWindowRect        = [ ("Full", singleWindowRect) ]
    }