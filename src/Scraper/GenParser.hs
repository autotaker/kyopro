{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Scraper.GenParser(Elem(..), Size(..), Shape(..), infer, main) where
import Scraper.Types
import Control.Monad
import qualified Data.Set as S 
import qualified Data.Map.Strict as M
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Control.Lens hiding(noneOf)
import Scraper.Parser(mainP)
import Text.Parsec hiding(token)
import Data.Text(Text)
import Data.Functor.Identity
import Debug.Trace

data Elem = Int32 | Int64 | Real | String | Unknown
    deriving(Eq, Ord, Show)
-- Lattice structure of Elem
--      String
--      /    \     
--     |    Int64
--    Real    |
--     |    Int32
--      \    /
--      Unknown
    
data Shape = Shape {
    shapeSize :: ![Size]
  , shapeElem :: !Elem
  } | ShapeElem { 
    shapeElem :: !Elem 
  }deriving(Show)

instance Semigroup Elem where
    Unknown <> x = x
    x <> Unknown = x
    String <> x    = String
    x    <> String = String
    Real <> Int32  = String
    Int32 <> Real  = String
    Real  <> Int64 = String
    Int64 <> Real  = String
    Int64 <> Int32 = Int64
    Int32 <> Int64 = Int64
    x <> y | x == y = x
    x  <>  y = error $ "unexpected pattern:" ++ show (x,y)

instance Monoid Elem where
    mempty = Unknown

instance Semigroup Shape where
    Shape l1 e1 <> Shape l2 e2 = Shape l1 (e1 <> e2)
    Shape l1 e1 <> ShapeElem e2 = Shape l1 (e1 <> e2)
    ShapeElem e1 <> Shape l2 e2 = Shape l2 (e1 <> e2)
    ShapeElem e1 <> ShapeElem e2 = ShapeElem (e1 <> e2)
    
data Size = SizeConst !Int | SizeVar !String
    deriving(Show)

data Value = VInt32 Int | VInt64 Int | VReal Double | VStr String 
    deriving(Show)
data Key = Key !String ![Int]
    deriving(Eq, Ord,Show)

toElem :: Value -> Elem
toElem (VInt32 _) = Int32
toElem (VInt64 _) = Int64
toElem (VReal _) = Real
toElem (VStr _) = String

type Input = M.Map Key Value
type Parser a = ParsecT Text Input Identity a

decimal32 :: Parser Int
decimal32 = do
    b <- option False (True <$ char '-')
    xs <- many1 digit
    many1 space
    let v = if b then -read xs else read xs
    unless (length xs <= 10 && abs v < 2^(31::Int)) $ 
        unexpected "the value is too large"
    pure v

decimal64 :: Parser Int
decimal64 = do
    b <- option False (True <$ char '-')
    xs <- many1 digit
    many1 space
    let v = if b then -read xs else read xs :: Integer
    unless (fromIntegral (minBound :: Int) <= v && v <= fromIntegral (maxBound :: Int)) $ 
        unexpected "the value is too large"
    pure $ fromInteger v

float :: Parser Double
float = do
    b <- option False (True <$ char '-')
    xs <- many1 digit
    ys <- char '.' *> many1 digit
    many1 space
    let s = xs ++ '.' : ys
    let v' = read s
        v = if b then -v' else v'
    pure v

lexeme :: Parser String
lexeme = do
    xs <- many1 (noneOf "\r\n \t")
    many1 space
    pure xs

token :: Parser Value
token = VInt32 <$> try decimal32 
    <|> VInt64 <$> try decimal64 
    <|> VReal <$> try float 
    <|> VStr <$> lexeme

parseInput :: Parser () -> String -> Text -> Either ParseError Input
parseInput p name s = runParser (p *> getState) M.empty name s

genParser :: [Pattern] -> Parser ()
genParser = mapM_ (goPtn M.empty)
    where
    goPtn !env (PatSingle xs) = mapM_ (goIdent env) xs
    goPtn env (PatHSep s) = goSeq env s
    goPtn env (PatVCat s) = goSeq env s
    goIdent !env (Ident x sub) = do
        key <- forM sub $ \case 
            One -> pure 1
            Two -> pure 2
            Var x -> do
                Just (VInt32 v) <- M.lookup (Key [x] []) <$> getState 
                pure v
            Param i -> pure $! env M.! i
        val <- token
        tbl <- getState
        putState $! M.insert (Key x key) val tbl
    goSeq !env Sequence{..} = do
        n <- case _seqEnd of
            Left i -> pure i
            Right x -> do
                Just (VInt32 v) <- M.lookup (Key [x] []) <$> getState
                pure v
        forM_ [_seqBegin..n] $ \i -> 
            goPtn (M.insert _seqVar i env) _seqPat
        
type Constraint = (String, Shape)
infer :: [Pattern] -> [Text] -> Either ParseError (M.Map String Shape)
infer ptns inputs = do
    let p = genParser ptns
        names = [ "input" ++ show i | i <- [1..] :: [Int] ]
    dicts <- zipWithM (parseInput p) names inputs
    let cs = (ptns >>= inferPtn M.empty) ++ (dicts >>= inferInput)
        inferInput dict = do
            (Key x _, v) <- M.toList dict 
            pure (x, ShapeElem (toElem v))
    pure $ M.fromListWith (<>) cs

inferPtn :: M.Map Int Size -> Pattern -> [Constraint]
inferPtn penv (PatSingle xs) = xs >>= inferIdent penv
inferPtn penv (PatHSep s) = inferSeq penv s
inferPtn penv (PatVCat s) = inferSeq penv s

inferIdent :: M.Map Int Size -> Ident -> [Constraint]
inferIdent penv (Ident x sub) = 
    (x, Shape size Unknown) : (sub >>= inferSub)
    where
    size = map (\case 
        Param i -> penv M.! i
        Var x -> SizeVar [x]
        One -> SizeConst 1
        Two -> SizeConst 2) sub

inferSub :: SubscriptElem -> [Constraint]
inferSub (Var ch) = pure ([ch], Shape [] Int32)
inferSub _ = []

inferSeq :: M.Map Int Size -> Sequence -> [Constraint]
inferSeq penv Sequence{ _seqEnd = en, _seqVar = i, _seqPat = p } = 
    [ ([ch], Shape [] Int32) | Right ch <- pure en ] ++ inferPtn penv' p
    where
        size = case en of
            Left s -> SizeConst s
            Right ch -> SizeVar [ch]
        penv' = M.insert i size penv

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
