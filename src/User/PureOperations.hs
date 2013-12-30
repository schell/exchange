{-# LANGUAGE OverloadedStrings #-}
module User.PureOperations where

import           Types
import qualified Data.Text as T


userAccount :: User -> Account
userAccount = ("user-" `T.append`) . T.pack . show . fromEnum . _userId


