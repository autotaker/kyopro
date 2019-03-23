{-# LANGUAGE OverloadedStrings #-}
module Main where

import Scraper.GenParser hiding (main)
import Scraper.Types
import qualified Generator.Haskell as Haskell
import qualified Generator.Cplusplus as Cpp
import qualified Data.Aeson as Aeson
import Control.Monad
import Control.Lens
import Data.Aeson.Lens
import Text.Parsec
import Scraper.Parser

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
                print $ Haskell.gen res ptns
                print $ Cpp.gen res ptns

