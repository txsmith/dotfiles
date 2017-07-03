{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Layout where

import XMonad

import XMonad.Layout.NoBorders
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Simplest
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed (addTabs)
import XMonad.Layout.TabBarDecoration (shrinkText)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.ResizableTile (ResizableTall(..))

import XMonad.Actions.Navigation2D (
    Navigation2DConfig(..)
    , centerNavigation
    , lineNavigation
    , singleWindowRect)
import XMonad.Hooks.ManageDocks

import Styles

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
  transform FULLBAR x k = k (avoidStruts Simplest) (const x)

myLayoutHook = smartBorders 
             $ fullScreenToggle
             $ fullBarToggle
             $ boringWindows
             $ hiddenWindows
             $ boringWindows
             $ avoidStruts (flex ||| tabs)
  where 
    tabs = named "Tabs"
            $ noFrillsDeco shrinkText topBarTheme
            $ addTabs shrinkText myTabTheme
            $ Simplest

    flex = trimNamed 5 "Flex"
            $ windowNavigation
            $ addTopBar
            $ addTabs shrinkText myTabTheme
            $ subLayout [] (Accordion ||| Simplest)
            $ standardLayouts
    
    standardLayouts = spacing gap 
                    $ myGaps 
                    $ (suffixed "Std 1/2" $ ResizableTall 1 (1/20) (1/2) [])
                    ||| (suffixed "Std 2/3" $ ResizableTall 1 (1/20) (2/3) [])
    
    fullBarToggle = mkToggle (single FULLBAR)
    
    fullScreenToggle = mkToggle (single FULL)
    
    addTopBar = noFrillsDeco shrinkText topBarTheme

    myGaps =  gaps [(U,gap), (D,gap), (R,gap), (L,gap)]

    named n = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                             (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n = renamed [(XMonad.Layout.Renamed.AppendWords n)]


myNav2DConf :: Navigation2DConfig 
myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [ ("Full", centerNavigation) ]
    , unmappedWindowRect        = [ ("Full", singleWindowRect) ]
    }