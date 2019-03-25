{-# LANGUAGE OverloadedStrings, RecordWildCards, BangPatterns, NoTypeApplications, LambdaCase, DeriveGeneric #-}
module Scraper.AtCoder( downloadTasks, AccountInfo(..) ) where

import           Control.Monad
import           Control.Applicative
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
import           Text.Printf
import           GHC.Generics
import           Data.Char(toLower)
import           Control.Monad.Logger
import           Control.Monad.Trans

rootURL = "https://atcoder.jp/"
loginURL = rootURL </> "/login"
contestRootURL contestId = "https://atcoder.jp/contests/" </> contestId
(</>) :: String -> String -> String
(</>) "" right = right
(</>) left "" = left
(</>) left right 
    | last left == '/' = left ++ dropWhile (=='/') right
    | head right == '/' = left ++ right
    | otherwise  = left ++ '/': right
    
data LoginForm = LoginForm {
    loginUserF :: !T.Text
  , loginPassF :: !T.Text
  , loginCSRFF :: !T.Text
  , loginCSRFToken :: !T.Text
  } deriving Show


data TaskInfo = TaskInfo {
    taskId          :: !T.Text
  , taskName        :: !T.Text
  , taskLink        :: !T.Text
  , taskTimeLimit   :: !T.Text
  , taskMemoryLimit :: !T.Text
  } deriving(Show, Generic)

myModifier :: String -> String -> String
myModifier prefix str = toLower ch : cs
    where
    ch:cs = drop (length prefix) str

instance Aeson.ToJSON TaskInfo where
  toEncoding = Aeson.genericToEncoding options
    where options = Aeson.defaultOptions {
        Aeson.fieldLabelModifier = myModifier "task" 
    }

instance Aeson.ToJSON TaskDetail where
  toEncoding = Aeson.genericToEncoding options
    where options = Aeson.defaultOptions {
        Aeson.fieldLabelModifier = myModifier "task" 
    }
instance Aeson.ToJSON SampleCase where
  toEncoding = Aeson.genericToEncoding options
    where options = Aeson.defaultOptions {
        Aeson.fieldLabelModifier = myModifier "sample" 
    }

data TaskDetail = TaskDetail {
    taskInfo :: !TaskInfo
  , taskStatement:: !T.Text
  , taskConstraint :: !T.Text
  , taskInputSpec :: !T.Text
  , taskInputSpecPre :: !T.Text
  , taskOutputSpec :: !T.Text
  , taskSampleCases :: [SampleCase]
  } deriving(Show, Generic)


data SampleCase = SampleCase {
    sampleInput :: !T.Text
  , sampleOutput ::  !T.Text
  } deriving(Show, Generic)

buildLoginForm :: LoginForm -> String -> String -> [ FormParam ]
buildLoginForm LoginForm{..} username passwd = 
    [ encodeUtf8 loginUserF := username
    , encodeUtf8 loginPassF := passwd
    , encodeUtf8 loginCSRFF := loginCSRFToken ]

parseTasks :: Scraper T.Text [TaskInfo]
parseTasks = chroots rowSelector parseRow
    where
    rowSelector = 
        "div" @: [ "id" @= "main-div" ] // "table" // "tbody" // "tr"
    parseRow = inSerial $ do
        (taskLink, taskId) <- 
            seekNext $ chroot "td" $ (,) <$> attr "href" "a" <*> text "a"
        taskName <- seekNext $ text ("td" // "a")
        taskTimeLimit <- seekNext $ text "td"
        taskMemoryLimit <- seekNext $ text "td"
        pure TaskInfo{..}

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

parseTaskDetail :: TaskInfo -> Scraper T.Text TaskDetail
parseTaskDetail taskInfo = chroot stmtJa parseStmt
    where
    stmtJa = "div"  @: [ "id"  @= "task-statement" ] 
             // "span" @: ["class" @= "lang-ja" ]
    parseStmt = inSerial $ do
        taskStatement <- seekNext $ innerHTML "section"
        taskConstraint <- seekNext $ innerHTML "section"
        let ioStyle = "div" @: [ "class" @= "io-style" ]
        let parseInputSpec = 
                (,) <$> innerHTML ("section" // "pre")
                    <*> innerHTML "section"
        ((taskInputSpecPre, taskInputSpec), taskOutputSpec) <- 
            (seekNext (chroot ioStyle $ inSerial $ 
                    (,) <$> seekNext parseInputSpec 
                        <*> seekNext (innerHTML "section")))
              <|> ((,) <$> seekNext parseInputSpec <*> seekNext (innerHTML "section"))
        taskSampleCases <- many $ do
            sampleInput <- seekNext $ text $ "section" // "pre"
            sampleOutput <- seekNext $ text $ "section" // "pre"
            pure SampleCase{..}
        pure TaskDetail{..}


scrapeResponse :: Response LBS.ByteString -> Scraper T.Text a -> Maybe a
scrapeResponse res action = scrapeStringLike htmlText action
    where
    htmlText = decodeUtf8 $ LBS.toStrict (res ^. responseBody)


login :: Session.Session -> String -> String -> LoggingT IO ()
login sess username passwd = do
    loginGetR <- liftIO $ Session.get sess loginURL
    let Just formTmpl = scrapeResponse loginGetR parseLoginForm
        !form = buildLoginForm formTmpl username passwd
    logDebugN $ "POST " <> T.pack loginURL
    loginPostRes <- liftIO $ Session.post sess loginURL form
    logDebugN $ "POST OK"
    let status = loginPostRes ^. responseStatus . statusCode
    logInfoN "Login Succeeded" 

tasks :: Session.Session -> String -> LoggingT IO [TaskInfo]
tasks sess contestId = do
    tasksGetR <- liftIO $ Session.get sess (contestRootURL contestId </> "tasks" )
    let Just tasks = scrapeResponse tasksGetR parseTasks
    pure tasks

taskDetail :: Session.Session -> TaskInfo -> LoggingT IO TaskDetail
taskDetail sess task = do
    let url = rootURL </> T.unpack (taskLink task)
    logDebugN $ "GET " <> T.pack url
    detailGetR <- liftIO $ Session.get sess url
    logDebugN $ "GET OK"
    let Just detail = scrapeResponse detailGetR (parseTaskDetail task)
    pure detail

data AccountInfo = AccountInfo { username :: String, password :: String }
downloadTasks :: Maybe AccountInfo -> String -> LoggingT IO ()
downloadTasks maccount contestId = do
    sess <- liftIO Session.newSession
    _ <- case maccount of
        Just AccountInfo{..} -> login sess username password
        Nothing -> pure ()
    tasks <- tasks sess contestId
    taskDetails <- forM tasks $ taskDetail sess
    liftIO $ Aeson.encodeFile "tasks.json" taskDetails
    
