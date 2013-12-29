{-# LANGUAGE OverloadedStrings #-}
module BtcUtils where

import Network.Bitcoin
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Text as T

bitcoinInfoToJSON :: BitcoindInfo -> Value
bitcoinInfoToJSON i = object [ "bitcoinVersion"    .= bitcoinVersion i
                             , "protocolVersion"   .= protocolVersion i
                             , "walletVersion"     .= walletVersion i
                             , "balance"           .= balance i
                             , "numBlocks"         .= numBlocks i
                             , "numConnections"    .= numConnections i
                             , "proxy"             .= proxy i
                             , "onTestNetwork"     .= onTestNetwork i
                             , "transactionFeePaid".= transactionFeePaid i
                             , "bitcoindErrors"    .= bitcoindErrors i
                             ]


blockToJSON :: Block -> Value
blockToJSON b = object [ "blockHash"         .= blockHash b
                       , "blkConfirmations"  .= blkConfirmations b
                       , "blkSize"           .= blkSize b
                       , "blkHeight"         .= blkHeight b
                       , "blkVersion"        .= blkVersion b
                       , "merkleRoot"        .= merkleRoot b
                       , "subTransactions"   .= subTransactions b
                       , "blkTime"           .= blkTime b
                       , "blkNonce"          .= blkNonce b
                       , "blkBits"           .= blkBits b
                       , "blkDifficulty"     .= blkDifficulty b
                       , "nextBlock"         .= nextBlock b
                       , "prevBlock"         .= prevBlock b
                       ]


txInfoToJSON :: RawTransactionInfo -> Value
txInfoToJSON t = object [ "raw"  .= raw t
                        , "txnVersion" .= txnVersion t
                        , "txnLockTime" .= txnLockTime t
                        , "vin" .= V.map txInToJSON (vin t)
                        , "vout" .= V.map txOutToJSON (vout t)
                        , "rawTxBlockHash" .= rawTxBlockHash t
                        , "rawBlockInfo" .= (" "::T.Text) --rawBlockInfo t
                        ]


txInToJSON :: TxIn -> Value
txInToJSON ti@(TxCoinbase _) = object [ "txCoinbase" .= txCoinbase ti ]

txInToJSON ti = object [ "txInId" .= txInId ti
                       , "numOut" .= numOut ti
                       , "scriptSig" .= show (scriptSig ti)
                       , "txSequence" .= txSequence ti
                       ]


txOutToJSON :: TxOut -> Value
txOutToJSON to = object [ "txoutVal" .= txoutVal to
                        , "scriptPubKey" .= show (scriptPubKey to)
                        ]

