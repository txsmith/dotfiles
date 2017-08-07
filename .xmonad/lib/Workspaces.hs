module Workspaces where

import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects

import Control.Monad
import Actions

wsBrowser = "Internet"
wsJava = "Java"
wsChat = "Chat"
wsSpotify = "Spotify"
wsTerminals = "Terminal"

myWorkspaces = [wsBrowser, wsTerminals, wsChat, wsJava, wsSpotify]

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
                , projectStartHook  = Just $ replicateM_ 3 (spawnOn wsTerminals myTerminal)
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
