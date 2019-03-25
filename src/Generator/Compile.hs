{-# LANGUAGE BangPatterns #-}
module Generator.Compile( ParserStmt(..), compile, declVars ) where

import Scraper.Types
import Scraper.GenParser
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Function

data ParserStmt =
    StmtPattern (Pattern Shape)
  | StmtDecl String Shape
  deriving(Show)

compile :: [Pattern Shape] -> [ParserStmt]
compile ptns = declVars ++ compilePtns shapeEnv S.empty ptns
    where
    shapeEnv = annotMap ptns
    declVars = 
        [ StmtDecl x sh | (x, sh) <- M.assocs shapeEnv, null $ shapeSize sh ]

declVars :: [ParserStmt] -> [(String, Shape)]
declVars l = [ (x, sh) | StmtDecl x sh <- l ]

compilePtns :: M.Map String Shape -> S.Set String -> [Pattern Shape] -> [ParserStmt]
compilePtns env scope [] = []
compilePtns env scope (PatSingle xs: ptns) = 
    StmtPattern (PatSingle xs): defs (compilePtns env scope'' ptns)
    where
    scope' = foldr S.insert scope $ map _identName xs
    (defs, scope'') = 
        env `M.withoutKeys` scope'
            & M.assocs
            & filter (\(k, s) -> 
                not (null (shapeSize s)) 
                && all saturated (shapeSize s))
            & foldl step (id, scope') 
    saturated (SizeConst _) = True
    saturated (SizeVar x) = S.member x scope'
    step (defs, !scope) (k, shape) = (defs', scope')
        where
        scope' = S.insert k scope
        defs' = defs . (def:)
        def = StmtDecl k shape
compilePtns env scope (ptn: ptns) = StmtPattern ptn : compilePtns env scope ptns

