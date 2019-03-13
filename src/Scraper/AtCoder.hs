{-# LANGUAGE OverloadedStrings, RecordWildCards, BangPatterns, NoTypeApplications #-}
module Scraper.AtCoder where

import           Control.Monad
import           Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8, encodeUtf8)
import           Data.Time
import           Network.Wreq
import qualified Network.Wreq.Session as Session
import           Text.HTML.Scalpel

loginURL = "https://atcoder.jp/login"

data LoginForm = LoginForm {
    loginUserF :: !T.Text
  , loginPassF :: !T.Text
  , loginCSRFF :: !T.Text
  , loginCSRFToken :: !T.Text
  } deriving Show

buildLoginForm :: LoginForm -> String -> String -> [ FormParam ]
buildLoginForm LoginForm{..} username passwd = 
    [ encodeUtf8 loginUserF := username
    , encodeUtf8 loginPassF := passwd
    , encodeUtf8 loginCSRFF := loginCSRFToken ]

parseLoginForm :: Scraper T.Text LoginForm
parseLoginForm = chroot loginForm parseForm
    where
    loginForm = "form" @: [ "class" @= "form-horizontal" ] 
    parseForm = do
        loginUserF <- attr "name" $ "input" @: [ "id" @= "username" ]
        loginPassF <- attr "name" $ "input" @: [ "id" @= "password" ]
        loginCSRFF <- attr "name" $ "input" @: [ "type" @= "hidden" ]
        loginCSRFToken <- attr "value" $ "input" @: [ "type" @= "hidden" ]
        pure $ LoginForm {..}

login :: Session.Session -> String -> String -> IO ()
login sess username passwd = do
    loginGetR <- Session.get sess loginURL
    let !htmlText = decodeUtf8 $ LBS.toStrict (loginGetR ^. responseBody) 
    let Just !form = scrapeStringLike htmlText parseLoginForm
    loginPostRes <- Session.post sess loginURL (buildLoginForm form username passwd)
    print loginPostRes

main :: IO ()
main = do
    sess <- Session.newSession
    username <- getLine
    passwd <- getLine
    login sess username passwd
