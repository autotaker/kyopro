module Main where

import Scraper.GenParser
import Scraper.Types
import Generator.Haskell

main :: IO ()
main = do
    Just v <- Aeson.decodeFileStrict "tasks.json" :: IO (Maybe Aeson.Value)
    forM_ (v ^.. values) $ \task -> do
        let [StringPrim src] = task ^.. key "inputSpecPre". _Primitive 
        let inputs = task ^.. key "sampleCases" . values . key "input" . _Primitive 
        case parse mainP "-" src of
            Left err -> print err
            Right ptns -> 
                print $ infer ptns [ x | StringPrim x <- inputs]

