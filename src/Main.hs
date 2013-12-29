{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import           Web.Scotty
import           Network.Bitcoin as BTC
import           Data.Configurator
import           Data.Acid
import           Data.Acid.Local
import           Types
import           Routes
import           DB


main :: IO ()
main = do
    (cfg, _) <- autoReload autoConfig [Required "btci.config"]
    url  <- lookupDefault "http://localhost:18332" cfg "url"
    user <- lookupDefault "bitcoinrpc" cfg "user"
    pass <- lookupDefault "bitcoinrpcpassword" cfg "password"
    let auth = Auth url user pass

    bracket (openLocalState initialUsers)
            createCheckpointAndClose
            (\acid -> bracket (openLocalState initialOrders)
                              createCheckpointAndClose
                              (startScotty auth acid))

startScotty :: Auth -> AcidState Users -> AcidState Orders -> IO ()
startScotty auth acidUsers acidOrders =
    scotty 18331 $ do
        userRoutes acidUsers
        btcRoutes auth
        orderRoutes acidOrders


