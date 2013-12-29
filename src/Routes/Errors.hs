{-# LANGUAGE OverloadedStrings #-}
module Routes.Errors where

import           Web.Scotty
import           Network.HTTP.Types.Status
import           Templates
import qualified Data.ByteString.Char8 as B
import qualified Text.Blaze.Html5 as H


errorRoutes :: ScottyM ()
errorRoutes =
    get "/error/:code" $ do
        code <- param "code"
        let stat = case code of
                       404 -> notFound404
                       403 -> forbidden403
                       _   -> mkStatus code "Other error"
        status stat
        blazePretty $ wrapper "Error" (H.toMarkup $ B.unpack $ statusMessage stat)


