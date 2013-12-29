module Tools.Interactive where

import           Crypto.BCrypt
import           Types
import           Data.Time.Clock
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

generateUserInteractive :: IO ()
generateUserInteractive = do
    putStr "\nName:"
    name  <- getLine

    putStr "\nId:"
    uid   <- getLine

    putStr "\nEmail (is not validated):"
    email <- getLine

    putStr "\nPassword:"
    passw <- getLine

    Just salt  <- genSaltUsingPolicy slowerBcryptHashingPolicy
    let saltedP = salt `B.append` B.pack passw
    Just hash  <- hashPasswordUsingPolicy slowerBcryptHashingPolicy saltedP

    t <- getCurrentTime
    
    print $ User { _userId = Id $ read uid
                 , _userName = T.pack name
                 , _userEmail = T.pack email
                 , _userPassword = hash
                 , _userSalt = salt 
                 , _userLastLogin = t
                 }

