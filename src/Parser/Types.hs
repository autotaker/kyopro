{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Types(
    Exp(..)
  , Ident(..)
  , Subscript
  , SubscriptElem(..)
  , Sequence(..)
  , Pattern(..)
  , _PatSingle, _PatHSep, _PatVCat
  , seqBegin, seqEnd, seqVar, seqPat
  , identName, identSub
  , annotPat, annotMap
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
import qualified Data.Map.Strict as M

data Exp = Id !(Ident ())
         | Dots | VDots
         deriving(Ord,Eq,Show)

type Subscript = [SubscriptElem]

data Ident a = Ident {
    _identName :: !String
  , _identSub  :: !Subscript
  , _identAnn  :: !a
  } deriving(Ord, Eq, Show, Generic, Functor, Foldable, Traversable)

data SubscriptElem = One | Two | Var Char | Param Int
    deriving(Ord,Eq,Show)

data Sequence a = Sequence {
    _seqBegin :: !Int
  , _seqEnd   :: !(Either Int Char)
  , _seqVar   :: !Int 
  , _seqPat   :: Pattern a
  } deriving(Show, Generic, Functor, Foldable, Traversable)

data Pattern a = PatSingle [Ident a]
               | PatHSep (Sequence a)
               | PatVCat (Sequence a)
               deriving(Show, Generic, Functor, Foldable ,Traversable)

makePrisms ''Pattern
makeLenses ''Sequence
makeLenses ''Ident

instance PP.Pretty SubscriptElem where
    pPrint One = PP.char '1'
    pPrint Two = PP.char '2'
    pPrint (Var ch) = PP.char ch
    pPrint (Param i) = PP.pPrint i

instance PP.Pretty (Ident a) where
    pPrint (Ident x [] _) = PP.text x
    pPrint (Ident x [s] _) = PP.text x PP.<> "_" PP.<> PP.pPrint s
    pPrint (Ident x sub _) = 
        PP.text x PP.<> "_" PP.<> 
            PP.braces (PP.hcat (PP.punctuate PP.comma (map PP.pPrint sub)))

instance PP.Pretty (Pattern a) where
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

substPat :: Int -> SubscriptElem -> Pattern a -> Pattern a
substPat i v = goPat 
    where
    goPat (PatSingle l) = PatSingle $ map goIdent l
    goPat (PatHSep s) = PatHSep $ s & seqPat %~ goPat
    goPat (PatVCat s) = PatVCat $ s & seqPat %~ goPat
    goIdent x = x & identSub %~ map goElem
    goElem (Param j) | j == i = v
    goElem x = x

annotMap :: [Pattern a] -> M.Map String a
annotMap ptns = M.fromList $ ptns >>= go
    where
    goIdent (Ident x _ ann) = pure (x, ann)
    go (PatSingle l) = l >>= goIdent
    go (PatHSep s) = go (_seqPat s)
    go (PatVCat s) = go (_seqPat s)

annotPat :: (Ident a -> Ident b) -> Pattern a -> Pattern b
annotPat f = go
    where
    go (PatSingle l) = PatSingle $! map f l
    go (PatHSep s) = PatHSep $! s & seqPat %~ go
    go (PatVCat s) = PatVCat $! s & seqPat %~ go
