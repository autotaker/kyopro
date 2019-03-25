{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

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

maybeError :: String -> Maybe a -> IO a
maybeError msg Nothing = ioError (userError msg)
maybeError _ (Just v) = pure v

eitherError :: Show e => Either e a -> IO a
eitherError (Left err) = ioError (userError (show err))
eitherError (Right v) = pure v

traceIt :: Show a => a -> a
traceIt x = traceShow x x

main :: IO ()
main = do
    config <- parseConfig
    taskFileExists <- doesFileExist "tasks.json"
    unless taskFileExists $ do
        maccount <- promptAccountInfo config
        Scraper.downloadTasks maccount (contestId config)
    v <- (Aeson.decodeFileStrict "tasks.json" :: IO (Maybe Aeson.Value)) >>= maybeError "tasks.json is broken"
    forM_ (v ^.. values) $ \task -> do
        let [StringPrim src] = task ^.. key "inputSpecPre". _Primitive 
        let [StringPrim taskId] = traceIt $ task ^.. key "info" . key "id" . _Primitive
        createDirectoryIfMissing False (T.unpack taskId)
        let inputs = task ^.. key "sampleCases" . values . key "input" . _Primitive 
        tmpl <- Render.parseYaml "render.yaml"
        case parse mainP (T.unpack taskId ++ ".inputSpecPre") src of
            Left err -> hPutStrLn stderr $ show err
            Right ptns -> do
                case infer ptns [ x | StringPrim x <- inputs] of
                    Left err -> hPutStrLn stderr $ show err
                    Right shapedPtns -> do
                        let prog = compile shapedPtns
                        let mainPath = T.unpack taskId ++ "/" ++ "main.cpp"
                        Render.render (T.unpack taskId) tmpl prog

