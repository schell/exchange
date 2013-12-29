{-# LANGUAGE OverloadedStrings #-}
module Routes.OrderServices where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Web.Scotty
import           Data.Maybe
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Time.Clock
import           Network.Bitcoin
import qualified Data.Text.Lazy as T
import           Order.DB
import           Types
import           Routes.Utils


orderRoutes :: AcidState Orders -> ScottyM ()
orderRoutes acid = do
    getOrders acid
    putOrder acid
    getPutOrder acid


getOrders :: AcidState Orders -> ScottyM ()
getOrders acid =
    get "/orders/:order" $ do
        kind   <- param "order" :: ActionM T.Text
        orders <- query' acid PeekOrders
        when (kind == "sell") $
            json $ _sellOrders orders
        when (kind == "buy") $
            json $ _buyOrders orders


putOrder :: AcidState Orders -> ScottyM ()
putOrder acid =
    put addOrderRoute $ addOrderAction acid


getPutOrder :: AcidState Orders -> ScottyM ()
getPutOrder acid = 
    get addOrderRoute $ addOrderAction acid


addOrderRoute :: RoutePattern
addOrderRoute = "/orders/:order/expires/:seconds/btc/:btc/usd/:usd"


addOrderAction :: AcidState Orders -> ActionM ()
addOrderAction acid = do
    order   <- param "order"
    seconds <- param "seconds" :: ActionM Integer
    btc     <- param "btc" :: ActionM Double
    usd     <- param "usd"
    addOrder acid order (fromIntegral seconds) (realToFrac btc) usd



addOrder :: AcidState Orders -> T.Text -> NominalDiffTime -> BTC -> USD -> ActionM ()
addOrder acid order seconds btc usd
    | order /= "sell" && order /= "buy" = json $ statusErr $ order `T.append` " is not a valid order type."
    | otherwise = do
        let ot = if order == "sell" then Sell else Buy
        t   <- liftIO getCurrentTime
        mId <- getUserId
        unless (isNothing mId) $ do
            order' <- update' acid $ InsertOrder (fromJust mId) ot (addUTCTime seconds t) btc usd
            json order'




--getBlockNotify acid =
--    get "/blocknotify/:blockhash" $ do
--        bhash <- param "blockhash"
--        addys <- query' acid (PeekOrdersWithStatus OrderNeedsSellerFunding)
--        liftIO $ forM_ addys $ \a -> do
--            mOrder <- query' acid $ PeekOrderWithSellerAddress a
--            unless (isNothing mOrder) $ do
--                 unspentTXs <- V.toList <$> listUnspent auth (Just 2) Nothing (return a)
--                 let unspent = sum $ map unspentAmount unspentTXs
--                 when (unspent > 0) $
--                     void $ update' acid (UpdateOrderWithSellerAddress a OrderNeedsBuyerFunding)
--        json $ object [ "status" .= ("ok" :: T.Text)
--                      , "blockHash" .= (bhash :: T.Text)
--                      ]

--putSellAt acid =
--    put "/sell/:exchangeRate" $ do
--        exchangeRate <- param "exchangeRate"
--        addy <- liftIO $ getNewAddress auth (Just "sell")
--        let order = newOrderFrom addy exchangeRate
--        liftIO $ update' acid $ AddOrder order
--        json order

