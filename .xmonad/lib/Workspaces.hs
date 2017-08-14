module Workspaces where

import XMonad.Core
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook (stringProperty, className, (=?), (<&&>))
import Actions

import Control.Monad (void)
import Data.Traversable (traverse)

wsBrowser = "Internet"
wsJava = "Java"
wsChat = "Chat"
wsSpotify = "Spotify"
wsTerminals = "Terminal"
wsHueHaskell = "Hue Haskell"
wsXMonad = "XMonad"
wsHaskellBook = "Haskell Book"

myWorkspaces = [wsBrowser, wsHueHaskell, wsHaskellBook, wsXMonad, wsTerminals, wsChat, wsJava, wsSpotify]

isScratchpadTerminal = (className =? "Gnome-terminal")
    <&&> (stringProperty "WM_WINDOW_ROLE" =? "Scratchpad")
    
isCalculator = (className =? "Gnome-calculator")

scratchpads = [
      NS "terminal" (myTerminal ++ " --role=Scratchpad") isScratchpadTerminal defaultFloating
    , NS "calculator" myCalculator isCalculator defaultFloating
    ] 

projects :: [Project]
projects =
    [ project wsJava "~/" [myTerminal, myIntelliJ]
    , project wsBrowser "~/" [myBrowser]
    , project wsTerminals "~/" [myTerminal]
    , project wsChat "~/" [myTelegram]
    , project wsSpotify "~/" [myMusic]
    , project wsHueHaskell "~/dev/haskell/hue" [myTerminal, "code ."]
    , project wsHaskellBook "~/dev/haskell/hue" [myTerminal, "code ."]
    , project wsHaskellBook "~/dev/haskell/hue" [myTerminal, "code .", "xo ~/Dropbox/Ebooks/haskell-programming-0.12.0-screen.pdf"]
    , project wsXMonad "~/.xmonad" [myTerminal, "code ."]
    ]

project :: ProjectName -> FilePath -> [String] -> Project
project ws dir spawnList =  Project {
    projectName       = ws
  , projectDirectory  = dir
  , projectStartHook  = Just $ void $ traverse (spawnOn ws) spawnList
}
