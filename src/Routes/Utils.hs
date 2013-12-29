{-# LANGUAGE OverloadedStrings #-}
module Routes.Utils where

import Data.Text.Lazy as T
import Data.Aeson
import Web.Scotty
import Types
import Session.Utils

statusOk :: Value
statusOk = object [ "status" .= ("ok" :: T.Text) ]

statusErr :: T.Text -> Value
statusErr o = object [ "status"  .= ("error" :: T.Text)
                     , "message" .= o
                     ]

getUserId :: ActionM (Maybe Id)
getUserId = do
    mCookie <- readUserCookie
    return $ fmap _cookieUserId mCookie 

