module Scraper.Parser where

import qualified Data.Text as T
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec

data Exp = Ident String (Maybe Subscript)
         | Dots | VDots
         deriving(Show)
type Subscript = [SubscriptElem]
data SubscriptElem = One | Two | Var Char 
    deriving(Show)

mathP, exprP :: Parser Exp
mathP = string "<var>" *> exprP <* string "</var>"
exprP = (Ident <$> identifier <*> optionMaybe subscriptP)
    <|> (Dots <$ dotsP) <|> (VDots <$ vdotsP)

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
vdotsP = string "\\vdots"

