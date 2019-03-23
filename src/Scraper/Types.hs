{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper.Types(
    Exp(..)
  , Ident(..)
  , Subscript
  , SubscriptElem(..)
  , Sequence(..)
  , Pattern(..)
  , _PatSingle, _PatHSep, _PatVCat
  , seqBegin, seqEnd, seqVar, seqPat
  , identName, identSub
) where

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

data Exp = Id !Ident
         | Dots | VDots
         deriving(Ord,Eq,Show)

type Subscript = [SubscriptElem]

data Ident = Ident {
    _identName :: String
  , _identSub  :: Subscript
  } deriving(Ord, Eq, Show, Generic)

data SubscriptElem = One | Two | Var Char | Param Int
    deriving(Ord,Eq,Show)

data Sequence = Sequence {
    _seqBegin :: Int
  , _seqEnd   :: Either Int Char
  , _seqVar   :: Int 
  , _seqPat   :: Pattern 
  } deriving(Show, Generic)

data Pattern = PatSingle [Ident]
             | PatHSep Sequence
             | PatVCat Sequence
             deriving(Show, Generic)

makePrisms ''Pattern
makeLenses ''Sequence
makeLenses ''Ident

instance PP.Pretty SubscriptElem where
    pPrint One = PP.char '1'
    pPrint Two = PP.char '2'
    pPrint (Var ch) = PP.char ch
    pPrint (Param i) = PP.pPrint i

instance PP.Pretty Ident where
    pPrint (Ident x []) = PP.text x
    pPrint (Ident x [s]) = PP.text x PP.<> "_" PP.<> PP.pPrint s
    pPrint (Ident x sub) = 
        PP.text x PP.<> "_" PP.<> 
            PP.braces (PP.hcat (PP.punctuate PP.comma (map PP.pPrint sub)))

instance PP.Pretty Pattern where
    pPrint (PatSingle xs) = PP.hsep $ map PP.pPrint xs
    pPrint (PatHSep Sequence{ 
        _seqBegin = s0
      , _seqEnd = Right ch
      , _seqVar = i
      , _seqPat = pat }) =
        PP.hsep [ PP.pPrint pat1, PP.pPrint pat2, "...", PP.pPrint patN ]
        where pat1 = substPat i One pat
              pat2 = substPat i Two pat
              patN = substPat i (Var ch) pat
    pPrint (PatVCat Sequence{ 
        _seqBegin = s0
      , _seqEnd = Right ch
      , _seqVar = i
      , _seqPat = pat }) =
        PP.vcat [ PP.pPrint pat1, PP.pPrint pat2, "...", PP.pPrint patN ]
        where pat1 = substPat i One pat
              pat2 = substPat i Two pat
              patN = substPat i (Var ch) pat

substPat :: Int -> SubscriptElem -> Pattern -> Pattern
substPat i v = goPat 
    where
    goPat (PatSingle l) = PatSingle $ map goIdent l
    goPat (PatHSep s) = PatHSep $ s & seqPat %~ goPat
    goPat (PatVCat s) = PatVCat $ s & seqPat %~ goPat
    goIdent x = x & identSub %~ map goElem
    goElem (Param j) | j == i = v
    goElem x = x
