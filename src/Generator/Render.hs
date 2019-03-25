{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Generator.Render(Render, render, parseYaml)  where

import Generator.Compile
import qualified Data.Yaml as Yaml
import qualified Text.Ginger as Ginger
import Data.Aeson hiding(String)
import GHC.Generics
import Data.Char
import Data.Function
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Scraper.Types
import Scraper.GenParser 

data Render = Render {
    renderType :: !RenderType
  , renderTerm :: !RenderTerm
  , renderTabstop :: !Int
} deriving(Generic)

newtype Template = Template { getTemplate :: Ginger.Template Ginger.SourcePos }

data RenderType = RenderType {
    renderInt32 :: !Template
  , renderInt64 :: !Template
  , renderReal  :: !Template
  , renderString :: !Template
  , renderArray :: !Template
  , renderUnknown :: !Template
} deriving(Generic)

data RenderTerm = RenderTerm {
    renderVar :: !Template
  , renderParam :: !Template
  , renderFor :: !Template
  , renderDecl :: !Template
  , renderScan :: !Template
  , renderMain :: !Template
} deriving(Generic)

instance FromJSON Template where
    parseJSON v = do
        s <- parseJSON v 
        let resolver x = pure Nothing
        r <- Ginger.parseGinger resolver Nothing s 
        case r of
            Left err -> fail $ show err
            Right v -> pure $ Template v

aesonOptions = defaultOptions {
    fieldLabelModifier = \l -> l & drop (length ("render"::String)) & \(c:cs) -> toLower c: cs
    }

instance FromJSON RenderType where
    parseJSON = genericParseJSON aesonOptions
instance FromJSON RenderTerm where
    parseJSON = genericParseJSON aesonOptions
instance FromJSON Render where
    parseJSON = genericParseJSON aesonOptions

parseYaml :: FilePath -> IO Render
parseYaml path = Yaml.decodeFileThrow path

render :: Render -> [ParserStmt] -> T.Text
render (Render{ renderTerm = RenderTerm{..}
              , renderType = RenderType{..}
              , renderTabstop = tabstop }) = goMain
    where
    indent level txt = T.replicate (level * tabstop) " " <> txt
    nest txt = T.init $ T.unlines $ f $ T.lines txt
        where
        f (head:ls) = head : map (indent 1) ls
        f [] = []

    goMain :: [ParserStmt] -> T.Text
    goMain prog = Ginger.easyRender dict (getTemplate renderMain)
        where
        dict = object [ "vars" .= [ ( goVar x
                                    , goShape sh
                                    , map goSize (shapeSize sh)) 
                                    | (x, sh) <- declVars prog ]
                      , "main" .= nest (T.concat (map goStmt prog)) ]

    goStmt :: ParserStmt -> T.Text
    goStmt (StmtPattern ptn) = goPtn ptn
    goStmt (StmtDecl x shape) = 
        Ginger.easyRender dict (getTemplate renderDecl)
        where
        dict = object [ "var" .= goVar x 
                      , "type" .= goShape shape
                      , "dim" .= map goSize (shapeSize shape)
                      , "elem"  .= goElem (shapeElem shape) ]

    goPtn :: Pattern Shape -> T.Text
    goPtn (PatHSep s) = goSeq s
    goPtn (PatVCat s) = goSeq s
    goPtn (PatSingle xs) = T.concat (map goIdent xs)

    goIdent :: Ident Shape -> T.Text
    goIdent (Ident x sub shape) = 
        Ginger.easyRender dict (getTemplate renderScan)
        where
        dict = object [ "var" .= goVar x
                      , "index" .= map goIndex sub
                      , "elem" .= goElem (shapeElem shape)
                      , "dim"  .= map goSize (shapeSize shape)
                      , "type" .= goShape shape ]

    goSeq :: Sequence Shape -> T.Text
    goSeq Sequence{..} = Ginger.easyRender dict (getTemplate renderFor)
        where
        dict = object [ "param" .= goParam _seqVar
                      , "end" .= dEnd
                      , "body" .= nest (goPtn  _seqPat) ]
        dEnd = case _seqEnd of
            Left n -> T.pack $ show n
            Right x -> goVar [x]

    goVar :: String -> T.Text
    goVar x = T.strip $ Ginger.easyRender dict (getTemplate renderVar)
        where
        dict = object [ "var" .= x ]

    goParam :: Int -> T.Text
    goParam i = T.strip $ Ginger.easyRender dict (getTemplate renderParam)
        where
        dict = object [ "param" .= i ]

    goShape :: Shape -> T.Text
    goShape (Shape [] e) = goElem e
    goShape (Shape l e) = T.strip $ Ginger.easyRender dict (getTemplate renderArray)
        where
        dict = object [ "elem" .= goElem e
                      , "dim" .= map goSize l ]
    goElem :: Elem -> T.Text
    goElem Int32 = T.strip $ Ginger.easyRender () (getTemplate renderInt32)
    goElem Int64 = T.strip $ Ginger.easyRender () (getTemplate renderInt64)
    goElem Real = T.strip $ Ginger.easyRender () (getTemplate renderReal)
    goElem String = T.strip $ Ginger.easyRender () (getTemplate renderString)
    goElem Unknown = T.strip $ Ginger.easyRender () (getTemplate renderUnknown)

    goIndex :: SubscriptElem -> T.Text
    goIndex One = "1"
    goIndex Two = "2"
    goIndex (Var n) = goVar [n]
    goIndex (Param i) = goParam i

    goSize :: Size -> T.Text
    goSize (SizeConst x) = T.pack (show x)
    goSize (SizeVar x) = goVar x

