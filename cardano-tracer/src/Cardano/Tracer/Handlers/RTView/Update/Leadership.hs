{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Leadership
  ( updateLeadershipHistory
  ) where

import           Control.Monad.Extra (whenJust)
import           Data.Time.Clock
import           Data.Text (unpack)
import           Text.Read (readMaybe)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Types

updateLeadershipHistory
  :: NodeId
  -> BlockchainHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateLeadershipHistory nodeId (ChainHistory cHistory) metricName metricValue now =
  case metricName of
    -- Slot when the node was a leader, but couldn't forge the block.
    "cardano.node.nodeCannotForge"       -> updateNodeCannotForge
    -- Slot when this node forged last block.
    "cardano.node.forgedSlotLast"        -> updateForgedSlotLast
    -- Slot when this node is leader.
    "cardano.node.nodeIsLeader"          -> updateNodeIsLeader
    -- Slot when this node made leadership check and concludes it's not leader.
    "cardano.node.nodeNotLeader"         -> updateNodeIsNotLeader
    -- Slot when invalid block was forged.
    "cardano.node.forgedInvalidSlotLast" -> updateForgedInvalidSlotLast
    -- Slot when the node adopted the block it forged.
    "cardano.node.adoptedSlotLast"       -> updateAdoptedSlotLast
    -- Slot when the node didn't adopted the block it forged, but the block was valid.
    "cardano.node.notAdoptedSlotLast"    -> updateNotAdoptedSlotLast
    -- Slot when the leadership check is started.
    "cardano.node.aboutToLeadSlotLast"   -> updateAboutToLeadSlotLast
    -- Slot when the leadership check is failed.
    "cardano.node.couldNotForgeSlotLast" -> updateCouldNotForgeSlotLast
    _ -> return ()
 where
  valueS = unpack metricValue

  updateNodeCannotForge =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now NodeCannotForgeData $ ValueI slotNum

  updateForgedSlotLast =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now ForgedSlotLastData $ ValueI slotNum

  updateNodeIsLeader =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now NodeIsLeaderData $ ValueI slotNum

  updateNodeIsNotLeader =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now NodeIsNotLeaderData $ ValueI slotNum

  updateForgedInvalidSlotLast =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now ForgedInvalidSlotLastData $ ValueI slotNum

  updateAdoptedSlotLast =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now AdoptedSlotLastData $ ValueI slotNum

  updateNotAdoptedSlotLast =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now NotAdoptedSlotLastData $ ValueI slotNum

  updateAboutToLeadSlotLast =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now AboutToLeadSlotLastData $ ValueI slotNum

  updateCouldNotForgeSlotLast =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now CouldNotForgeSlotLastData $ ValueI slotNum
