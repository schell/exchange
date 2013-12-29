{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
module Order.Types where

import           Control.Applicative
import           Network.Bitcoin as BTC
import           Data.Monoid
import           Data.SafeCopy
import           Data.Data
import           Data.Aeson hiding (json)
import           Data.Time.Clock
import           Types.Types
import qualified Data.Text as T
import qualified Data.Set as S


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


type OrderCollection = S.Set Order


data Ledger = Ledger { _needsFunding :: OrderCollection
                     , _needsMatch   :: OrderCollection
                     , _expired      :: OrderCollection
                     , _cancelled    :: OrderCollection
                     } deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''Ledger)


instance ToJSON Ledger where
    toJSON (Ledger fs ms ss xs) =
        object [ "needsFunding" .= fs
               , "needsMatch" .= ms
               , "expired" .= ss
               , "cancelled" .= xs
               ]


initialLedger :: Ledger
initialLedger = Ledger S.empty S.empty S.empty S.empty


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

