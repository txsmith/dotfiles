{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Styles where 
import GHC.Word
import XMonad.Prompt
import XMonad.Layout.Tabbed
import XMonad.Layout.Decoration

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
white   = "#ffffff"

gap :: Int
gap    = 15

topbar = 4
border = 0
prompt = 24
status = 20

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

myFont = "xft:SFNS Display:style=Regular:size=11"

myNormalBorderColor  = "#455A64"
myNormalTextColor    = "#5E717A"
myHighlightTextColor = "#20A294"
xmobarCurrentWorkspaceColor = "#Af745f"

myBorderWidth :: Word32
myBorderWidth = 0


topBarTheme :: XMonad.Layout.Decoration.Theme
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = white
    , decoHeight            = topbar
    }

myTabTheme :: XMonad.Layout.Tabbed.Theme
myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

hotPromptTheme :: XPConfig
hotPromptTheme = myPromptTheme
    { bgColor  = red
    , fgColor  = base3
    , position = Top
    }