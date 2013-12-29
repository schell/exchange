{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Order.DB where

--import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Reader
import           Network.Bitcoin
import           Data.Acid
import           Data.Time.Clock
import           Types
import           Order.PureOperations


insertOrder :: UTCTime -> Id -> OrderType -> UTCTime -> BTC -> USD -> Update Orders Order
insertOrder now uid ot t btc usd = do
    invalidateExpiredOrders now
    orders <- get
    let (order, orders') = pureInsertOrder uid ot t btc usd orders
    put orders'
    return order


invalidateExpiredOrders :: UTCTime -> Update Orders ()
invalidateExpiredOrders now = do
    orders <- get
    put $ pureExpireOrders now orders
    return ()


--
--
--updateOrderWithSellerAddress :: Address -> OrderStatus -> Update Orders (Maybe Order)
--updateOrderWithSellerAddress a s = do
--    orders <- get
--    put $ M.update (\x -> Just x{_status=s}) a orders
--    M.lookup a <$> get
--
--
--peekOrderWithSellerAddress :: Address -> Query Orders (Maybe Order)
--peekOrderWithSellerAddress a = M.lookup a <$> ask
--
--
peekOrders :: Query Orders Orders
peekOrders = ask
--
--
--peekOrdersWithStatus :: OrderStatus -> Query Orders [Address]
--peekOrdersWithStatus s = do
--   orders <- M.elems <$> ask
--   return $ map _sellerAddress $ filter (\o -> _status o == s) orders
--
--
--newOrderFrom :: Address -> Double -> Order
--newOrderFrom a r = Order { _exchangeRate = r
--                         , _sellerAddress = a
--                         , _buyerAddress = Nothing
--                         , _status = OrderNeedsFunding
--                         }
--
--
$(makeAcidic ''Orders ['peekOrders, 'insertOrder, 'invalidateExpiredOrders])


