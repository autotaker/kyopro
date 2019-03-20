{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Scraper.Parser as Parser
import Text.Parsec
import Control.Monad

main :: IO ()
main = hspec $ do
    describe "Parser.mathP" $ do
        let samples = [ ("<var>A</var>", Parser.Ident "A" Nothing)
                      , ("<var>A_1</var>", Parser.Ident "A" (Just [Parser.One]))
                      , ("<var>A_2</var>", Parser.Ident "A" (Just [Parser.Two]))
                      , ("<var>A_N</var>", Parser.Ident "A" (Just [Parser.Var 'N']))
                      , ("<var>...</var>", Parser.Dots)
                      , ("<var>\\vdots</var>", Parser.VDots) ]
        forM_ samples $ \(input, answer) -> do
            it ("can parse " ++ show input) $ 
                parse Parser.mathP "-" input `shouldBe` Right answer 

