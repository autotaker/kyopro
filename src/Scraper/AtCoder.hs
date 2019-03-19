{-# LANGUAGE OverloadedStrings, RecordWildCards, BangPatterns, NoTypeApplications, LambdaCase #-}
module Scraper.AtCoder where

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
  } deriving(Show)

data TaskDetail = TaskDetail {
    taskInfo :: !TaskInfo
  , taskStatement:: !T.Text
  , taskConstraint :: !T.Text
  , taskInputSpec :: !T.Text
  , taskOutputSpec :: !T.Text
  , taskSampleCases :: [SampleCase]
  } deriving(Show)

data SampleCase = SampleCase {
    sampleInput :: !T.Text
  , sampleOutput ::  !T.Text
  } deriving(Show)

defaultTask :: TaskInfo
defaultTask = 
    TaskInfo { 
        taskId = "NA"
      , taskName = "NA"
      , taskLink = "NA"
      , taskTimeLimit = "NA"
      , taskMemoryLimit = "NA" 
    }

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
    taskIdC link v acc = acc { taskId = v, taskLink = link }
    taskNameC link v acc = acc { taskName = v, taskLink = link }
    taskTimeLimitC v acc = acc { taskTimeLimit = v }
    taskMemoryLimitC v acc = acc { taskMemoryLimit = v }

    buildTask fieldsC = foldr1 (.) fieldsC defaultTask
    parseRow = 
        fmap buildTask $ chroots "td" $ position >>= \case
            0 -> taskIdC <$> attr "href" "a" <*> text "a"
            1 -> taskNameC <$> attr "href" "a" <*> text "a"
            2 -> taskTimeLimitC <$> text anySelector
            3 -> taskMemoryLimitC <$> text anySelector
            _ -> pure id

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
        taskInputSpec <- seekNext $ innerHTML "section"
        taskOutputSpec <- seekNext $ innerHTML "section"
        taskSampleCases <- many $ do
            sampleInput <- seekNext $ text $ "section" // "pre"
            sampleOutput <- seekNext $ text $ "section" // "pre"
            pure SampleCase{..}
        pure TaskDetail{..}


scrapeResponse :: Response LBS.ByteString -> Scraper T.Text a -> Maybe a
scrapeResponse res action = scrapeStringLike htmlText action
    where
    htmlText = decodeUtf8 $ LBS.toStrict (res ^. responseBody)


login :: Session.Session -> String -> String -> IO ()
login sess username passwd = do
    loginGetR <- Session.get sess loginURL
    let Just formTmpl = scrapeResponse loginGetR parseLoginForm
        !form = buildLoginForm formTmpl username passwd
    loginPostRes <- Session.post sess loginURL form
    let status = loginPostRes ^. responseStatus . statusCode
    printf "Login Succeeded (%d)\n" status

tasks :: Session.Session -> String -> IO [TaskInfo]
tasks sess contestId = do
    tasksGetR <- Session.get sess (contestRootURL contestId </> "tasks" )
    let Just tasks = scrapeResponse tasksGetR parseTasks
    pure tasks

taskDetail :: Session.Session -> TaskInfo -> IO TaskDetail
taskDetail sess task = do
    let url = rootURL </> T.unpack (taskLink task)
    detailGetR <- Session.get sess url
    let Just detail = scrapeResponse detailGetR (parseTaskDetail task)
    pure detail

main :: IO ()
main = do
    sess <- Session.newSession
    username <- getLine
    passwd <- getLine
    contestName <- getLine
    login sess username passwd
    tasks <- tasks sess contestName
    print tasks
    forM_ tasks $ \task -> do
        l <-taskDetail sess task
        print l
