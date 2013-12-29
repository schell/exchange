{-# LANGUAGE OverloadedStrings #-}
module Routes.UserServices where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Web.Scotty
import           Data.Maybe
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Time.Clock
import           Data.Aeson hiding (json)
import qualified Data.Text.Lazy as T
import           User.DB
import           Session.Utils
import           Types
import           Templates


userRoutes :: AcidState Users -> ScottyM ()
userRoutes acid = do
    getIndex
    getLogin
    postLogin acid
    getSession
    getHome
    getUser acid

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
    get "/session" $ do
        authorizeAdmin
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


getUser :: AcidState (EventState PeekUserWithId) -> ScottyM ()
getUser acid =
    get "/user/:user" $ do
        authorize
        uid <- param "user"
        mUser  <- query' acid $ PeekUserWithId $ Id uid
        unless (isNothing mUser) $
            json $ fromJust mUser

