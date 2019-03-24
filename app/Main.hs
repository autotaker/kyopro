{-# LANGUAGE OverloadedStrings #-}
module Main where

import Scraper.GenParser hiding (main)
import Scraper.Types
import qualified Generator.Render as Render
import Generator.Compile
import qualified Data.Aeson as Aeson
import Control.Monad
import Control.Lens
import Data.Aeson.Lens
import Text.Parsec
import Scraper.Parser
import qualified Data.Text.IO as T

main :: IO ()
main = do
    Just v <- Aeson.decodeFileStrict "tasks.json" :: IO (Maybe Aeson.Value)
    forM_ (v ^.. values) $ \task -> do
        let [StringPrim src] = task ^.. key "inputSpecPre". _Primitive 
        let inputs = task ^.. key "sampleCases" . values . key "input" . _Primitive 
        case parse mainP "-" src of
            Left err -> print err
            Right ptns -> do
                Right res <- pure $ infer ptns [ x | StringPrim x <- inputs]
                print res
                let prog = compile res ptns
                tmpl <- Render.parseYaml "render.yaml"
                T.putStr $ Render.render res tmpl prog

