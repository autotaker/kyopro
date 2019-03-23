{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Generator.Haskell where

import Prelude hiding ((<>))
import Scraper.GenParser 
import Scraper.Types
import qualified Data.Map.Strict as M
import Text.PrettyPrint.HughesPJClass
import Data.Function((&))

gen :: M.Map String Shape -> [Pattern] -> Doc
gen shapeEnv = nest 4 . genPtns shapeEnv M.empty

type T a = a

tuple :: [Doc] -> Doc
tuple [x] = x
tuple l = parens . sep . punctuate comma $ l

genPtns :: M.Map String Shape -> M.Map String String -> [Pattern] -> Doc
genPtns env scope [] = freezes $+$ "pure" <+> tuple (map text $ M.elems scope)
    -- varA <- unsafeFreeze varA
    -- ...
    -- varM <- unsafeFreeze varM
    -- pure (varW, varH, varA)
    where
    freezes = vcat $ do
        (key, Shape (_:_) _) <- M.assocs env
        let v = text $ scope M.! key
        pure $ v <+> "<-" <+> "unsafeFreeze" <+> v

genPtns env scope (PatSingle xs:ptns) = 
    vcat (map (genIdent env scope) xs) 
    $+$ defs
    $+$ genPtns env scope'' ptns
    where
    scope' = foldr (\x acc -> M.insert x (convVar x) acc) scope $ map _identName xs
    (defs, scope'') = 
        env M.\\ scope'
            & M.assocs
            & filter (\(k, s) -> 
                not (null (shapeSize s)) 
                && all saturated (shapeSize s))
            & foldl step (empty, scope') 
    saturated (SizeConst _) = True
    saturated (SizeVar x) = M.member x scope'
    step (!doc, !scope) (k, shape) = (doc', scope')
        where
        scope' = M.insert k var scope
        var = convVar k 
        doc' = doc $+$ def
        -- varX <- newArray ((1,1), (H,W)) 
        n = length $ shapeSize shape
        def = text var <+> "<-" <+> "newArray_"
            <+> tuple [ tuple (replicate n "1")
                      , tuple (map ppSize $ shapeSize shape) ]
    ppSize :: Size -> Doc
    ppSize (SizeConst x) = pPrint x
    ppSize (SizeVar x) = text $ scope' M.! x
genPtns env scope (PatHSep s:ptns) = genSeq env scope s $+$ genPtns env scope ptns
genPtns env scope (PatVCat s:ptns) = genSeq env scope s $+$ genPtns env scope ptns

genPtn env scope (PatHSep s) = genSeq env scope s
genPtn env scope (PatVCat s) = genSeq env scope s
genPtn env scope (PatSingle xs) = vcat (map (genIdent env scope) xs) 

genSeq env scope Sequence{..} =
    -- forM_ [1..N] $ \i -> do
    --   ...
    "forM_" <+> brackets (dBegin <> ".." <> dEnd) 
            <+> "$" <+> "\\" <+> convParam _seqVar <+> "->" <+> "do" $+$
    nest 2 (genPtn env scope _seqPat)
    where
    dBegin = pPrint _seqBegin
    dEnd = case _seqEnd of
        Left n -> int n
        Right x -> text $ scope M.! [x]

typeOfShape :: Shape -> Bool -> Doc
typeOfShape (Shape [] e) _ = typeOfElem e
typeOfShape (Shape l  e) mutable = dtype <+> dix <+> typeOfElem e
    where
    dtype = case (mutable, e) of
        (True, String) -> "STArray s"
        (True, _) -> "STUArray s"
        (False, String) -> "Array"
        (False, _) -> "UArray"
    dix = tuple [ "Int" | _ <- l ]
typeOfElem String = "String"
typeOfElem Int32 = "Int32"
typeOfElem Int64 = "Int32"
typeOfElem Real  = "Double"

genIdent env scope (Ident x []) = text (convVar x) <+> "<-" <+> parseFunc e
    where
    -- varX <- scanInt
    e = shapeElem $ env M.! x
genIdent env scope (Ident x sub) =
    parseFunc e <+> ">>=" <+> "writeArray" <+> text (convVar x) <+> dsub
    -- scanInt >>= writeArray varX (i,j)
    where
    Shape size e = env M.! x
    dsub = tuple $ map f sub 
    f One = "1"
    f Two = "2"
    f (Var n) = text $ convVar [n]
    f (Param i) = convParam i

convVar :: String -> String
convVar x = "var" ++ x

convParam :: Int -> Doc
convParam i = "i" <> int i

parseFunc :: Elem -> Doc
parseFunc Int32 = "scanInt32"
parseFunc Int64 = "scanInt64"
parseFunc Real  = "scanDouble"
parseFunc String = "scanString"
parseFunc Unknown = "scanUnknown"

-- [W H
-- , Seq 1 H i (Seq 1 W j (A_{i,j})) ]
--  =>
-- do
--  varW <- scanInt 
--  varH <- scanInt
--  varA <- newUArray ((1,1), (H,W)) undefined
--  forM_ [1..h] $ \i -> 
--      forM_ [1..w] $ \j -> do
--          scanInt >>= writeArray varA (i,j) 
--
    


