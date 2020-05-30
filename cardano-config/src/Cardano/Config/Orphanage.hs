{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Orphanage () where

import           Cardano.Prelude
import qualified Prelude

import           Data.Aeson
import           Network.Socket (PortNumber)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Scientific (coefficient)
import qualified Data.Text as Text

import           Cardano.BM.Data.Tracer (TracingVerbosity(..))
import qualified Cardano.Chain.Update as Update
import           Ouroboros.Consensus.NodeId (NodeId(..), CoreNodeId (..))
import           Ouroboros.Network.Block
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.PBFT
import           Ouroboros.Consensus.Mock.Ledger.Block.Praos
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Mock.Ledger.Block.BFT
import           Ouroboros.Consensus.Protocol.BFT

instance HasHeader (Serialised (SimpleBlock SimpleMockCrypto (SimplePBftExt SimpleMockCrypto PBftMockCrypto))) where
  blockHash      = panic ""
  blockPrevHash  = panic ""
  blockSlot      = panic ""
  blockNo        = panic ""
  blockInvariant = panic ""

instance Measured BlockMeasure (Serialised (SimpleBlock SimpleMockCrypto (SimplePBftExt SimpleMockCrypto PBftMockCrypto))) where
  measure = panic ""

instance HasHeader (Serialised (SimpleBlock SimpleMockCrypto (SimplePraosExt SimpleMockCrypto PraosMockCrypto))) where
  blockHash      = panic ""
  blockPrevHash  = panic ""
  blockSlot      = panic ""
  blockNo        = panic ""
  blockInvariant = panic ""

instance Measured BlockMeasure (Serialised (SimpleBlock SimpleMockCrypto (SimplePraosExt SimpleMockCrypto PraosMockCrypto))) where
  measure = panic ""


instance HasHeader (Serialised (SimpleBlock SimpleMockCrypto (SimpleBftExt SimpleMockCrypto BftMockCrypto))) where
  blockHash      = panic ""
  blockPrevHash  = panic ""
  blockSlot      = panic ""
  blockNo        = panic ""
  blockInvariant = panic ""

instance Measured BlockMeasure (Serialised (SimpleBlock SimpleMockCrypto (SimpleBftExt SimpleMockCrypto BftMockCrypto))) where
  measure = panic ""

deriving instance Show TracingVerbosity

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> panic $ "Parsing of TracingVerbosity failed, "
                 <> err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = panic $ "Parsing of TracingVerbosity failed due to type mismatch. "
                             <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

instance FromJSON NodeId where
  parseJSON v = CoreId . CoreNodeId <$> parseJSON v


instance FromJSON PortNumber where
  parseJSON (Number portNum) = case readMaybe . show $ coefficient portNum of
                                 Just port -> pure port
                                 Nothing -> panic $ (show portNum)
                                                  <> " is not a valid port number."
  parseJSON invalid  = panic $ "Parsing of port number failed due to type mismatch. "
                             <> "Encountered: " <> (Text.pack $ Prelude.show invalid)

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    panic $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> (Text.pack $ Prelude.show invalid)
