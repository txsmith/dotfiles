module Workspaces where

import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook (stringProperty, className, (=?), (<&&>))
import Actions

import Control.Monad (void)
import Data.Traversable (traverse)

wsBrowser = "Internet"
-- wsJava = "Java"
wsChat = "Chat"
wsSpotify = "Spotify"
wsTerminals = "Terminal"
wsHueHaskell = "Hue Haskell"
wsXMonad = "XMonad"
wsHaskellBook = "Haskell Book"
wsStudy = "Study"

myWorkspaces = [
    wsBrowser
  , wsHueHaskell
  , wsHaskellBook
  , wsXMonad
  , wsChat
  -- , wsTerminals
  -- , wsJava
  , wsSpotify
  , wsStudy]

isScratchpadTerminal = (className =? "Gnome-terminal")
  <&&> (stringProperty "WM_WINDOW_ROLE" =? "Scratchpad")
    
isCalculator = (className =? "Gnome-calculator")

scratchpads = [
    NS "terminal" (myTerminal ++ " --role=Scratchpad") isScratchpadTerminal defaultFloating
  , NS "calculator" myCalculator isCalculator defaultFloating
  ] 

projects :: [Project]
projects = [
    -- project wsJava "~/" [myTerminal, myIntelliJ]
    project wsBrowser "~/" [myBrowser]
  -- , project wsTerminals "~/" [myTerminal]
  , project wsChat "~/" [myTelegram]
  , project wsSpotify "~/" [myMusic]
  , project wsHueHaskell "~/dev/haskell/hue" [myTerminal, "code ."]
  , project wsHaskellBook "~/dev/haskell/hue" [myTerminal, "code ."]
  , project wsHaskellBook "~/dev/haskell/learning/playground" ["xdg-open ~/Dropbox/Ebooks/haskell-programming-0.12.0-screen.pdf", myTerminal, "code ."]
  , project wsXMonad "~/.xmonad" [myTerminal, "code ."]
  , project wsStudy "~/dev/tu" [myTerminal, "code .", myBrowser ++ " --new-window \"https://brightspace.tudelft.nl\""]
  ]

project :: ProjectName -> FilePath -> [String] -> Project
project ws dir spawnList =  Project {
    projectName       = ws
  , projectDirectory  = dir
  , projectStartHook  = Just $ void $ traverse (spawnOn ws) spawnList
}
