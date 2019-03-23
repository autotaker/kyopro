{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Generator.Cplusplus where

import Prelude hiding ((<>))
import Scraper.GenParser 
import Scraper.Types
import qualified Data.Map.Strict as M
import Text.PrettyPrint.HughesPJClass
import Data.Function((&))
import Data.List(tails)

gen :: M.Map String Shape -> [Pattern] -> Doc
gen shapeEnv ptns = 
    -- int main(void)
    -- {
    --   <main>
    -- }
    "int" <+> "main(void)" $+$
    "{" $+$ nest 2 (genPtns shapeEnv M.empty ptns $+$ "return 0;") $+$ "}"

type T a = a

tuple :: [Doc] -> Doc
tuple [x] = x
tuple l = parens . sep . punctuate comma $ l

genPtns :: M.Map String Shape -> M.Map String String -> [Pattern] -> Doc
genPtns env scope [] = "solve" <> tuple (map text $ M.elems scope) <> semi
    -- solve(varW, varH, varA)
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
        -- vector< vector<int> > varX(H, vector<int>(W, int(0))); 
        n = length $ shapeSize shape
        docTy = typeOfShape shape
        elt = shapeElem shape
        docInit = foldr (\(v, sh) acc -> 
            ppSize v <> comma <+> typeOfShape sh <> parens (acc))
            "0" [ (v, Shape l elt) | v:l <- tails $ shapeSize shape ]
        def = docTy <+> text var <> parens docInit <> semi
    ppSize :: Size -> Doc
    ppSize (SizeConst x) = pPrint x
    ppSize (SizeVar x) = text $ scope' M.! x

genPtns env scope (PatHSep s:ptns) = genSeq env scope s $+$ genPtns env scope ptns
genPtns env scope (PatVCat s:ptns) = genSeq env scope s $+$ genPtns env scope ptns

genPtn env scope (PatHSep s) = genSeq env scope s
genPtn env scope (PatVCat s) = genSeq env scope s
genPtn env scope (PatSingle xs) = vcat (map (genIdent env scope) xs) 

genSeq env scope Sequence{..} =
    -- for( auto i = 0; i < N; i++ ) { 
    --   ...
    -- }
    "for" <+> parens ("auto" <+> convParam _seqVar <+> "=" <+> "0"<> semi <+>
                      convParam _seqVar <+> "<" <+> dEnd <> semi <+>
                      "++"<> convParam _seqVar) $+$
    "{" $+$ nest 2 (genPtn env scope _seqPat) $+$ "}"
    where
    dBegin = pPrint _seqBegin
    dEnd = case _seqEnd of
        Left n -> int n
        Right x -> text $ scope M.! [x]

typeOfVector :: Elem -> Int -> Doc
typeOfVector e 0 = typeOfElem e
typeOfVector e n = "std::vector<" <> typeOfVector e (n-1) <> ">"

typeOfShape :: Shape -> Doc
typeOfShape (Shape l  e) = 
    typeOfVector e (length l) 
typeOfElem String = "string"
typeOfElem Int32 = "int"
typeOfElem Int64 = "long long"
typeOfElem Real  = "double"

genIdent env scope (Ident x []) = 
    typeOfShape sh <+> text (convVar x) <> semi 
    <+> "std::cin" <+> ">>" <+> text (convVar x) <> semi
    where
    sh = env M.! x
    -- std::cin >> varX;
genIdent env scope (Ident x sub) =
    "std::cin" <+> ">>" <+> text (convVar x) <> dsub <> semi
    -- std::cin >> varX[i][j]
    where
    Shape size e = env M.! x
    dsub = hcat $  map (brackets . f) sub 
    f One = "1"
    f Two = "2"
    f (Var n) = text $ convVar [n]
    f (Param i) = convParam i

convVar :: String -> String
convVar x = "var" ++ x

convParam :: Int -> Doc
convParam i = "i" <> int i

-- [W H
-- , Seq 1 H i (Seq 1 W j (A_{i,j})) ]
--  =>
-- do
--  int varW, varH;
--  std::cin >> varW;
--  std::cin >> varH;
--  vector< vector<int> > A(H, vector<int>(W))
--  varA <- newUArray ((1,1), (H,W)) undefined
--  forM_ [1..h] $ \i -> 
--      forM_ [1..w] $ \j -> do
--          scanInt >>= writeArray varA (i,j) 
--
    


