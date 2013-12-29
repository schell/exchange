{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
module Order.Types where

import           Control.Applicative
import           Network.Bitcoin as BTC
import           Data.Monoid
import           Data.SafeCopy
import           Data.Data
import           Data.Aeson hiding (json)
import           Data.Time.Clock
import qualified Data.Text as T
import           Types.Types


type USD = Double


data RawOrder = RawOrder { _orderId        :: Id
                         , _orderOwner     :: Id
                         , _orderExpires   :: UTCTime
                         , _orderAmount    :: BTC
                         , _orderThreshold :: USD
                         } deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''RawOrder)


instance ToJSON RawOrder where
    toJSON (RawOrder (Id i) (Id ii) e a t) =
        object [ "id"        .= i
               , "owner"     .= ii
               , "expires"   .= e
               , "amount"    .= a
               , "threshold" .= t
               ]


instance FromJSON RawOrder where
    parseJSON (Object v) = RawOrder         <$>
                           v .: "id"        <*>
                           v .: "owner"     <*>
                           v .: "expires"   <*>
                           v .: "amount"    <*>
                           v .: "threshold"
    parseJSON _ = mempty


data OrderType = Sell | Buy deriving (Show, Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''OrderType)


data Order = Order OrderType RawOrder deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''Order)


instance ToJSON Order where
    toJSON (Order Sell o) = object [ "type" .= ("Sell" :: T.Text)
                                   , "order" .= o
                                   ]

    toJSON (Order Buy o) = object [ "type" .= ("Buy" :: T.Text)
                                  , "order" .= o
                                  ]


data LedgerNeedsFunding = LedgerNeedsFunding [Order] deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''LedgerNeedsFunding)

data LedgerNeedsMatch = LedgerNeedsMatch [Order] deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''LedgerNeedsMatch)

data LedgerSuccessful = LedgerSuccessful [Order] deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''LedgerSuccessful)

data LedgerFailure = LedgerFailure [Order] deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''LedgerFailure)


data Ledger = Ledger { _needsFunding :: LedgerNeedsFunding
                     , _needsMatch   :: LedgerNeedsMatch
                     , _successful   :: LedgerSuccessful
                     , _failure      :: LedgerFailure
                     } deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''Ledger)


instance ToJSON Ledger where
    toJSON (Ledger (LedgerNeedsFunding fs) (LedgerNeedsMatch ms) (LedgerSuccessful ss) (LedgerFailure xs)) =
        object [ "needsFunding" .= fs
               , "needsMatch" .= ms
               , "success" .= ss
               , "failure" .= xs
               ]


initialLedger :: Ledger
initialLedger = Ledger (LedgerNeedsFunding []) (LedgerNeedsMatch []) (LedgerSuccessful []) (LedgerFailure [])


data Orders = Orders { _buyOrders   :: Ledger
                     , _sellOrders  :: Ledger
                     , _nextOrderId :: Id
                     } deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''Orders)


initialOrders :: Orders
initialOrders = Orders { _buyOrders = initialLedger
                       , _sellOrders = initialLedger
                       , _nextOrderId = Id 0
                       }

