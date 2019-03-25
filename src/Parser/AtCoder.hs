{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.AtCoder(
    module Parser.Types
  , mathP, mainP ) where

import qualified Data.Text as T
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Lens
import GHC.Generics hiding(Selector)
import qualified Text.PrettyPrint.HughesPJClass as PP
import Parser.Types

newtype Selector a where
    Sel :: Traversal' (Pattern a) SubscriptElem -> Selector a

enumSelector :: Pattern a -> [Selector a]
enumSelector = goPat id
    where
    goPat :: Traversal' (Pattern a) (Pattern a) -> Pattern a -> [Selector a]
    goPat sel (PatSingle l) = goIdent (sel . _PatSingle) l
    goPat sel (PatHSep s) = goPat (sel . _PatHSep . seqPat) (_seqPat s)
    goPat sel (PatVCat s) = goPat (sel . _PatVCat . seqPat) (_seqPat s)
    goIdent :: Traversal' (Pattern a) [Ident a] -> [Ident a] -> [Selector a]
    goIdent _ [] = []
    goIdent sel (x:xs) = 
        goSub (sel . _head . identSub) (_identSub x) 
            ++ goIdent (sel . _tail) xs
    goSub :: Traversal' (Pattern a) Subscript -> Subscript -> [Selector a]
    goSub sel [] = []
    goSub sel (x:xs) = 
        [Sel $ sel . _head | x == One] 
            ++ goSub (sel . _tail) xs

degree :: Pattern a -> Int
degree (PatSingle _) = 0
degree (PatHSep Sequence{ _seqPat = p }) = degree p + 1
degree (PatVCat Sequence{ _seqPat = p }) = degree p + 1

matchSeq :: Pattern a -> Maybe (Pattern a) -> Pattern a -> Maybe (Sequence a)
matchSeq pat1 mpat2 patN = do
    let sels = filter p $ enumSelector pat1
        isVar (Var _) = True
        isVar _ = False
        p = case mpat2 of
            Just pat2 -> \(Sel sel) -> 
                anyOf sel (==Two) pat2 && anyOf sel isVar patN
            Nothing -> \(Sel sel) -> anyOf sel isVar patN
    Sel sel0:_ <- pure sels
    [Var n] <- pure (toListOf sel0 patN)
    let i = degree pat1
    let pat = foldl (\ !acc (Sel sel) -> acc & sel .~ Param i) pat1 sels
    pure Sequence {
        _seqBegin = 1
      , _seqEnd = Right n
      , _seqVar = degree pat1
      , _seqPat = pat }

-- A_{11} A_{12} ... A_{1M}
-- A_{21} A_{22} ... A_{2M}
-- \vdots
-- A_{N1} A_{N2} ... A_{NM}
-- >>>
-- Sequence 1 'N' 0 (PatHSep (Sequence 1 'M' 1 (PatSingle [PatVar "A" 2])))
-- A_{11} A_{12} ... A_{1M}
-- >>>
-- Sequence 1 'M' (\x -> PatSingle [PatVar "A" [One, x]])
-- A_{21} A_{22} ... A_{2M}
-- >>>
-- Sequence 1 'M' (\x -> PatSingle [PatVar "A" [Two, x]])
-- A_{N1} A_{N2} ... A_{NM}
-- >>>
-- Sequence 1 'M' (\x -> PatSingle [PatVar "A" [Var 'n', x]])

mathP, exprP :: Parser Exp
mathP = varP exprP
exprP = (Id <$> identP)
    <|> (Dots <$ dotsP) <|> (VDots <$ vdotsP)

varP :: Parser a -> Parser a
varP p = string "<var>" *> p <* string "</var>"

identP :: Parser (Ident ())
identP = Ident <$> identifier <*> option [] subscriptP <*> pure ()

lineP :: Parser (Pattern ())
lineP = (try finiteP <|> try sequenceP) <* (void crlf <|> eof)
    where
    sp = many1 (char ' ')
    finiteP = PatSingle <$> sepBy1 (varP identP) sp
    sequenceP = do
        var1 <- varP identP <* sp 
        mvar2 <- optionMaybe (varP identP <* sp)
        _ <- varP dotsP <* sp
        varN <- varP identP
        let pat1 = PatSingle [var1]
            pat2 = fmap (PatSingle . (:[])) mvar2
            patN = PatSingle [varN]
        Just s <- pure $ matchSeq pat1 pat2 patN
        pure $ PatHSep s

specP :: Parser (Pattern ())
specP = try vcatP <|> lineP
    where
    vcatP = do
        pat1 <- lineP
        pat2 <- optionMaybe lineP
        varP vdotsP <* crlf
        patN <- lineP
        Just s <- pure $ matchSeq pat1 pat2 patN
        pure $ PatVCat s

mainP :: Parser [Pattern ()]
mainP = many specP <* eof

subscriptP :: Parser Subscript
subscriptP = 
    char '_' *> doit 
    where doit = ((:[]) <$> elemP) 
                <|> (char '{' *> many elemP <* char '}')

elemP :: Parser SubscriptElem
elemP = One <$ char '1' <|> Two <$ char '2' <|> Var <$> letter

identifier :: Parser String
identifier = many1 letter

dotsP = string "..."
vdotsP = string "\\vdots" <|> string ":"
