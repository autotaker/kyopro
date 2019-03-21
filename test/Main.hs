{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Scraper.Parser as P
import Text.Parsec
import Control.Monad

main :: IO ()
main = hspec $ do
    describe "Parser.mathP" $ do
        let samples = [ 
                ("<var>A</var>", P.Id (P.Ident "A" []))
              , ("<var>A_1</var>", P.Id (P.Ident "A" [P.One]))
              , ("<var>A_2</var>", P.Id (P.Ident "A" [P.Two]))
              , ("<var>A_N</var>", P.Id (P.Ident "A" [P.Var 'N']))
              , ("<var>...</var>", P.Dots)
              , ("<var>\\vdots</var>", P.VDots) 
              ]
        forM_ samples $ \(input, answer) -> do
            it ("can parse " ++ show input) $ 
                parse P.mathP "-" input `shouldBe` Right answer 

