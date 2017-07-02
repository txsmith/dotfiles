{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Styles where 
import GHC.Word
import XMonad.Prompt

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
green       = "#859900"

gap    = 10
topbar = 10
border = 0
prompt = 20
status = 20

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

myFont = "xft:Source Code Pro:style=Regular:pixelsize=12:hinting=true"


myNormalBorderColor  = "#455A64"
myNormalTextColor    = "#5E717A"
myHighlightTextColor = "#20A294"
xmobarCurrentWorkspaceColor = "#Af745f"

myBorderWidth :: Word32
myBorderWidth = 2


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
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }