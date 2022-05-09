{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Peers
  ( updatePeers
  ) where

import           Control.Monad
import           Control.Monad.Extra (whenJustM)
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Peers
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

updatePeers
  :: UI.Window
  -> NodeId
  -> Peers
  -> DisplayedElements
  -> Text
  -> UI ()
updatePeers window nodeId@(NodeId anId) peers displayedElements trObValue =
  if "NodeKernelPeers" `T.isInfixOf` trObValue
    then return () -- It was empty 'TraceObject' (without useful info), ignore it.
    else do
      let peersParts = T.splitOn "," trObValue
          peersNum = length peersParts
      -- Update peers number.
      setDisplayedValue nodeId displayedElements (anId <> "__node-peers-num") $ showT peersNum
      -- Update particular info about peers.
      forM_ peersParts $ \peerPart ->
        case T.words peerPart of
          [peerAddr, status, slotNo, reqsInF, blocksInF, bytesInF] -> do
            let idPrefix = anId <> peerAddr
            peerIsHere <- liftIO $ doesPeerExist peers nodeId peerAddr
            if peerIsHere
              then
                -- Peer is already displayed, so we have to update its values only.
                setPeerData idPrefix status slotNo reqsInF blocksInF bytesInF
              else do
                -- This is new peer, so remember it and update its values.
                liftIO $ addPeer peers nodeId peerAddr
                addPeerRow idPrefix peerAddr status slotNo reqsInF blocksInF bytesInF
          _ -> return () -- It's strange: wrong format of peers info, ignore it.
 where
  setPeerData idPrefix status slotNo reqsInF blocksInF bytesInF =
    setTextValues
      [ (idPrefix <> "__status",    status)
      , (idPrefix <> "__slotNo",    slotNo)
      , (idPrefix <> "__reqsInF",   reqsInF)
      , (idPrefix <> "__blocksInF", blocksInF)
      , (idPrefix <> "__bytesInF",  bytesInF)
      ]

  addPeerRow idPrefix peerAddr status slotNo reqsInF blocksInF bytesInF = do
    let idPrefix' = unpack idPrefix
    whenJustM (UI.getElementById window (unpack anId <> "__node-peers-tbody")) $ \el ->
      void $ element el #+
        [ UI.tr #+
            [ UI.td #+
                [ UI.span ## (idPrefix' <> "__address")
                          # set text (unpack peerAddr)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__status")
                          # set text (unpack status)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__slotNo")
                          # set text (unpack slotNo)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__reqsInF")
                          # set text (unpack reqsInF)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__blocksInF")
                          # set text (unpack blocksInF)
                ]
            , UI.td #+
                [ UI.span ## (idPrefix' <> "__bytesInF")
                          # set text (unpack bytesInF)
                ]
            ]
        ]
