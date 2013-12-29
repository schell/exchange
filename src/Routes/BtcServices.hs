{-# LANGUAGE OverloadedStrings #-}
module Routes.BtcServices where

import           Control.Monad.IO.Class (liftIO)
import           Web.Scotty
import           Network.Bitcoin as BTC
import           BtcUtils


btcRoutes :: Auth -> ScottyM ()
btcRoutes = getBtcInfo


getBtcInfo :: Auth -> ScottyM ()
getBtcInfo auth =
    get "/btcinfo" $ do
        info <- liftIO $ getBitcoindInfo auth
        json $ bitcoinInfoToJSON info



