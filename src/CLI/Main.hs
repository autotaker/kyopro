{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module CLI.Main(main) where

import Generator.Infer
import Parser.Types
import Parser.AtCoder
import qualified Scraper.AtCoder as Scraper
import qualified Generator.Render as Render
import Generator.Compile

import qualified Data.Aeson as Aeson
import Control.Monad
import Control.Lens
import Data.Aeson.Lens
import Text.Parsec
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Directory
import Debug.Trace
import Paths_kyopro
import Control.Monad.Logger
import Control.Monad.Except

import Control.Exception

data Flag = LoginRequired | RenderTemplate FilePath | ShowHelp
    deriving(Eq)

data Config = Config {
    loginRequired :: Bool
  , renderTemplate :: FilePath
  , contestId :: String
}

options :: [OptDescr Flag]
options =
    [ Option ['r'] ["render"] (ReqArg RenderTemplate "FILE") "specify your render template" 
    , Option ['l'] ["login"] (NoArg LoginRequired) "enable login"
    , Option ['h'] ["help"] (NoArg ShowHelp) "show help"
    ]
defaultConfig contest = Config {
    loginRequired = False
  , renderTemplate = "render.yaml"
  , contestId = contest
}

parseConfig :: IO Config
parseConfig = do
    args <- getArgs
    let header = "Usage: kyopro [Option..] <contestID>"
    case getOpt Permute options args of
        (opts, [name], []) 
            | ShowHelp `elem` opts -> do
                putStrLn $ usageInfo header options
                exitSuccess
            | otherwise -> pure $! foldl step (defaultConfig name) opts
                where
                    step acc LoginRequired = acc { loginRequired = True }
                    step acc (RenderTemplate path) = acc { renderTemplate = path }
                    step acc ShowHelp = acc
        (_, _, errs) -> do
            putStrLn $ concat errs ++ usageInfo header options
            exitFailure

promptAccountInfo :: Config -> IO (Maybe Scraper.AccountInfo)
promptAccountInfo Config{ loginRequired = False } = pure Nothing
promptAccountInfo _ = do
    putStr "username:"
    hFlush stdout
    username <- getLine
    putStr "password:"
    hFlush stdout
    password <- withEcho False getLine
    putChar '\n'
    hFlush stdout
    pure $ Just Scraper.AccountInfo{..}

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

maybeError :: Monad m => String -> Maybe a -> ExceptT String m a
maybeError msg Nothing = throwError msg 
maybeError _ (Just v) = pure v


traceIt :: Show a => a -> a
traceIt x = traceShow x x

findYaml :: String -> IO String
findYaml path = do
    b <- doesFileExist path
    if b 
        then pure path
        else do
            path <- Paths_kyopro.getDataFileName ("template/" ++ path)
            pure path

main :: IO ()
main = do
    config <- parseConfig
    runStdoutLoggingT $ do
        r <- runExceptT (doit config)
        case r of
            Left err -> logErrorN $ T.pack err
            Right v -> pure v
    where 
    doit config = do
        taskFileExists <- liftIO $ doesFileExist "tasks.json"
        unless taskFileExists $ do
            logInfoN $ "task.json doesn't exists. Trying to scrape it from the contest page."
            maccount <- liftIO $ promptAccountInfo config
            liftIO $ Scraper.downloadTasks maccount (contestId config)
        when taskFileExists $ logInfoN "\"tasks.json\" found. Skippin scraping from the contest page. If you want to rescrape it, please delete \"tasks.json\"."

        v <- liftIO (Aeson.decodeFileStrict "tasks.json" :: IO (Maybe Aeson.Value))  >>= maybeError "tasks.json is broken"
        tmpl <- liftIO $ findYaml (renderTemplate config) >>= Render.parseYaml 
        let handleByLogging action = catchError action (\e -> logErrorN $ T.pack e)
        forM_ (v ^.. values) $ \task -> handleByLogging $ do
            src <- case task ^.. key "inputSpecPre". _Primitive of
                [StringPrim src] -> pure src
                _ -> throwError "the value for key \"inputSpecPre\" is invalid"
            taskId <- case task ^.. key "info" . key "id" . _Primitive of
                [StringPrim taskId] -> pure taskId
                _ -> throwError "the value for key \"info.id\" is invalid"
            logInfoN $ "creating directory: "  <> taskId
            liftIO $ createDirectoryIfMissing False (T.unpack taskId)
            let inputs = task ^.. key "sampleCases" . values . key "input" . _Primitive 
            ptns <- withExceptT show $ liftEither $ parse mainP (T.unpack taskId ++ ".inputSpecPre") src
            shapedPtns <- withExceptT show $ liftEither $ infer ptns [ x | StringPrim x <- inputs] 
            let prog = compile shapedPtns
            let mainPath = T.unpack taskId ++ "/" ++ "main.cpp"
            liftIO $ Render.render (T.unpack taskId) tmpl prog

