module Workspaces where

import XMonad.StackSet as W
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook (stringProperty, className, (=?), (<&&>))
import Control.Monad
import Actions

wsBrowser = "Internet"
wsJava = "Java"
wsChat = "Chat"
wsSpotify = "Spotify"
wsTerminals = "Terminal"

myWorkspaces = [wsBrowser, wsTerminals, wsChat, wsJava, wsSpotify]

isScratchpadTerminal = (className =? "Gnome-terminal")
    <&&> (stringProperty "WM_WINDOW_ROLE" =? "Scratchpad")
    
isCalculator = (className =? "Gnome-calculator")

scratchpads = [
      NS "terminal" (myTerminal ++ " --role=Scratchpad") isScratchpadTerminal defaultFloating
    , NS "calculator" myCalculator isCalculator defaultFloating
    ] 

projects :: [Project]
projects =

    [ 
      Project   { projectName       = wsBrowser
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do
                    spawnOn wsJava myBrowser
                }

    , Project   { projectName       = wsJava
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do
                    spawnOn wsJava myIntelliJ
                    spawnOn wsJava myTerminal
                }
    
    , Project   { projectName       = wsTerminals
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ spawnOn wsTerminals myTerminal
                }

    , Project   { projectName       = wsChat
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do 
                    spawnOn wsChat myTelegram
                    -- spawnOn wsChat mySlack
                }

    , Project   { projectName       = wsSpotify
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ spawnOn wsSpotify myMusic
                }

    ]
