{-# LANGUAGE OverloadedStrings #-}
module Routes.UserServices where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Web.Scotty
import           Network.HTTP.Types.Status
import           Data.Maybe
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Time.Clock
import           Data.Aeson hiding (json)
import qualified Network.Bitcoin as BTC
import qualified Data.Text.Lazy as T
import           User.DB
import           User.PureOperations
import           Session.Utils
import           Types
import           Templates


userRoutes :: Auth -> AcidState Users -> ScottyM ()
userRoutes auth acid = do
    getIndex
    getLogin
    postLogin acid
    getSession
    getHome
    getUser auth acid
    getNewAddress auth acid

getIndex :: ScottyM ()
getIndex =
    get "/" $ blazePretty $ wrapper "Home" "Stuff"


getLogin :: ScottyM ()
getLogin =
    get "/login" $ blazePretty login


postLogin :: AcidState (EventState PeekUserWithEmail) -> ScottyM ()
postLogin acid =
    post "/login" $ do
        email <- param "email"
        pass  <- param "password"

        regdUser <- query' acid $ PeekUserWithEmail email
        when (isNothing regdUser) $ redirect "/login"

        -- Compare the user's password and stuff.
        let u = fromJust regdUser
        unless (userWithPasswordIsValid u pass) $
            redirect "/login"

        -- Set our cookie now that the user is authentic.
        -- writeUserCookie will take care of the expiry for us.
        writeUserCookie $ UserCookie (_userId u) zeroDay
        -- Update the last user login.
        t <- liftIO getCurrentTime
        update' acid $ UpdateUser $ u{_userLastLogin=t}
        redirect "/home"


getSession :: ScottyM ()
getSession =
    get "/session" $ authorizeAdmin $ do
        mCookie <- readUserCookie
        unless (isNothing mCookie) $ do
            let c = fromJust mCookie
            e <- cookieHasExpired c
            json $ object [ "cookie"     .= c
                          , "hasExpired" .= e
                          ]

getHome :: ScottyM ()
getHome =
    get "/home" $ do
        authorize
        json $ object ["status" .= ("ok"::T.Text)]


ifAuthorizedPeekUser :: AcidState Users -> (User -> ActionM ()) -> ActionM ()
ifAuthorizedPeekUser acid f =
    authorizeAndId $ \uid -> do
        mUser  <- query' acid $ PeekUserWithId uid
        maybe (jsonErr forbidden403) f mUser


getUser :: Auth -> AcidState (EventState PeekUserWithId) -> ScottyM ()
getUser auth acid =
    get "/account" $ ifAuthorizedPeekUser acid $ \user -> do
        let account = userAccount user
        bling <- liftIO $ BTC.getBalance' auth account
        addys <- liftIO $ BTC.getAddressesByAccount auth account
        json $ object [ "user" .= user
                      , "balance" .= bling
                      , "addresses" .= addys
                      ]


getNewAddress :: Auth -> AcidState Users -> ScottyM ()
getNewAddress auth acid =
    get "/newAddress" $ ifAuthorizedPeekUser acid $ \user -> do
        addy <- liftIO $ BTC.getNewAddress auth (Just $ userAccount user)
        json $ object [ "status"  .= ("ok" :: T.Text)
                      , "address" .= addy
                      ]
