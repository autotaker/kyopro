{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import qualified Scraper.Parser as P
import Text.Parsec
import Control.Monad
import Data.Either

main :: IO ()
main = hspec $ do
    describe "Parser.mathP" $ do
        let samples = [ 
                ("<var>A</var>", P.Id (P.Ident "A" [] ()))
              , ("<var>A_1</var>", P.Id (P.Ident "A" [P.One] ()))
              , ("<var>A_2</var>", P.Id (P.Ident "A" [P.Two] ()))
              , ("<var>A_N</var>", P.Id (P.Ident "A" [P.Var 'N'] ()))
              , ("<var>...</var>", P.Dots)
              , ("<var>\\vdots</var>", P.VDots) 
              ]
        forM_ samples $ \(input, answer) -> do
            it ("can parse " ++ show input) $ 
                parse P.mathP "-" input `shouldBe` Right answer 
    describe "Parser.mainP" $ do
        let inputs = 
                [ "<var>H</var> <var>W</var>\r\n<var>h</var> <var>w</var>\r\n"
                , "<var>N</var> <var>M</var> <var>C</var>\r\n<var>B_1</var> <var>B_2</var> <var>...</var> <var>B_M</var>\r\n<var>A_{11}</var> <var>A_{12}</var> <var>...</var> <var>A_{1M}</var>\r\n<var>A_{21}</var> <var>A_{22}</var> <var>...</var> <var>A_{2M}</var>\r\n<var>\\vdots</var>\r\n<var>A_{N1}</var> <var>A_{N2}</var> <var>...</var> <var>A_{NM}</var>\r\n"
                , "<var>N</var> <var>M</var>\r\n<var>A_1</var> <var>B_1</var>\r\n<var>A_2</var> <var>B_2</var>\r\n<var>\\vdots</var>\r\n<var>A_N</var> <var>B_N</var>\r\n"
                , "<var>A</var> <var>B</var>\r\n" ]
        forM_ inputs $ \input -> do
            it ("can parse " ++ show input) $
                parse P.mainP "-" input `shouldSatisfy` isRight

