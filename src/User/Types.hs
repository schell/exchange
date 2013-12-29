{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
module User.Types where

import           Data.Data
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.SafeCopy
import           Data.Aeson
import           Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import           Types.Types as Id


data User = User { _userId         :: Id
                 , _userName       :: T.Text
                 , _userEmail      :: T.Text
                 , _userPassword   :: B.ByteString
                 , _userSalt       :: B.ByteString
                 , _userLastLogin  :: UTCTime
                 } deriving (Show, Ord, Eq, Typeable)
$(deriveSafeCopy 0 'base ''User)


instance ToJSON User where
    toJSON (User _ n e _ _ l) = object [ "userName"  .= n
                                       , "userEmail" .= e
                                       , "userLastLogin" .= l
                                       ]


data UserCookie = UserCookie { _cookieUserId  :: Id
                             , _cookieExpires :: UTCTime
                             } deriving (Show, Eq, Ord)


instance ToJSON UserCookie where
    toJSON (UserCookie i utc) = object [ "userId" .= i
                                       , "expires" .= utc
                                       ]


instance FromJSON UserCookie where
    parseJSON (Object v) = do
        i   <- v .: "userId"
        utc <- v .: "expires"
        return $ UserCookie i utc

    parseJSON _ = mzero


data Users = Users { _usersNextId :: Integer
                   , _usersMap    :: M.Map Id User
                   } deriving (Show, Ord, Eq, Typeable)
$(deriveSafeCopy 0 'base ''Users)


initialUsers :: Users
initialUsers = Users { _usersNextId = 1
                     , _usersMap = foldl (\m u -> M.insert (_userId u) u m) M.empty $
                         defaultAdmin : defaultUsers
                     }


defaultAdmin :: User
defaultAdmin = User { _userId = Id 0
                    , _userName = "schell"
                    , _userEmail = "efsubenovex@gmail.com"
                    , _userPassword = "$2y$14$netZkdtew47pO5DcsUOOC.8na2CwghiujFkO9ZX4W03xD.4T3j432"
                    , _userSalt = "$2y$14$ycGDchE4ofOly/aBZVmTHO"
                    , _userLastLogin = zeroDay
                    }


defaultUsers :: [User]
defaultUsers = [ User { _userId = Id 1
                      , _userName = "john"
                      , _userEmail = "john.doe@mailinator.com"
                      , _userPassword = "$2y$14$J2t6RJAWFgvROu//xAzj6Orqesfy9pwuQ2xIjoMWL5jBblJUYTvhy"
                      , _userSalt = "$2y$14$2aOVN/UZpBbs.t0lz/EA0."
                      , _userLastLogin = zeroDay
                      }
               , User { _userId = Id 2
                      , _userName = "bob"
                      , _userEmail = "bobby.doe@mailinator.com"
                      , _userPassword = "$2y$14$ZqOfdZ6OrI4yW9jRgl2wVek2yYgNkwWLjGTJDmphhqvFyeRB9UYZ."
                      , _userSalt = "$2y$14$Rhr/HeUSy7vGRFrfh/hW.O"
                      , _userLastLogin = zeroDay
                      }
               ]


zeroDay :: UTCTime
zeroDay = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

