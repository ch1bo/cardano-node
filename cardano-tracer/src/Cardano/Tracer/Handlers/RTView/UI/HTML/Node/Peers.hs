{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Peers
  ( mkPeersTable
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPeersTable :: String -> UI Element
mkPeersTable anId = do
  closeIt <- UI.button #. "delete"
  peerTable <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card" #+
          [ UI.header #. "modal-card-head rt-view-peer-head" #+
              [ UI.p #. "modal-card-title rt-view-peer-title" #+
                  [ string "Peers of "
                  , UI.span ## (anId <> "__node-name-for-peers")
                            #. "has-text-weight-bold"
                            # set text anId
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-peer-body" #+
              [ UI.div ## (anId <> "__peer-table-container") #. "table-container" #+
                  [ UI.table ## (anId <> "__peer-table") #. "table rt-view-peer-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr #+
                              [ UI.th #+ [UI.span # set text "Address"]
                              , UI.th #+ [UI.span # set text "Status"]
                              , UI.th #+ [UI.span # set text "Slot no."]
                              , UI.th #+
                                  [ string "Req"
                                  , image "has-tooltip-multiline has-tooltip-top rt-view-what-icon" whatSVG
                                          # set dataTooltip "Number of requests in flight"
                                  ]
                              , UI.th #+ [UI.mkElement "abbr" # set UI.title__ "Blocks in flight" # set text "Blk"]
                              , UI.th #+ [UI.mkElement "abbr" # set UI.title__ "Bytes in flight" # set text "Bts"]
                              ]
                          ]
                      , UI.mkElement "tbody" ## (anId <> "__node-peers-tbody") #+ []
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element peerTable #. "modal"
  return peerTable
