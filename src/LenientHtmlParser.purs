module LenientHtmlParser where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, elem)
import Data.Monoid (mempty)
import Data.String (trim, fromCharArray)
import Text.Parsing.StringParser (Parser, ParseError, runParser, fail)
import Text.Parsing.StringParser.Combinators (fix, many, many1, manyTill)
import Text.Parsing.StringParser.String (anyChar, char, eof, noneOf, satisfy, string)

newtype TagName = TagName String
derive instance eqTagName :: Eq TagName
derive instance genericRepTagName :: Generic TagName _
instance showTagName :: Show TagName where show = genericShow

type Attributes = List Attribute

newtype Name = Name String
derive instance eqName :: Eq Name
derive instance genericRepName :: Generic Name _
instance showName :: Show Name where show = genericShow

newtype Value = Value String
derive instance eqValue :: Eq Value
derive instance genericRepValue :: Generic Value _
instance showValue :: Show Value where show = genericShow

data Attribute = Attribute Name Value
derive instance eqAttribute :: Eq Attribute
derive instance genericRepAttribute :: Generic Attribute _
instance showAttribute :: Show Attribute where show = genericShow

data Tag
  = TagOpen TagName Attributes
  | TagSingle TagName Attributes
  | TagClose TagName
  | TNode String
  | TScript Attributes String
derive instance eqTag :: Eq Tag
derive instance genericRepTag :: Generic Tag _
instance showTag :: Show Tag where show = genericShow

flattenChars :: List Char -> String
flattenChars = trim <<< fromCharArray <<< fromFoldable

comment :: Parser Unit
comment = do
  _ <- string "<!--"
  _ <- manyTill anyChar $ string "-->"
  pure unit

doctype :: Parser Unit
doctype = do
  _ <- string "<!DOCTYPE" <|> string "<!doctype"
  _ <- manyTill anyChar $ string ">"
  pure unit

skipSpace :: Parser Unit
skipSpace = fix \_ ->
  (comment *> skipSpace)
  <|> (doctype *> skipSpace)
  <|> (many1 ws *> skipSpace)
  <|> pure unit
  where
    ws = satisfy \c ->
      c == '\n' ||
      c == '\r' ||
      c == '\t' ||
      c == ' '

lexeme :: forall p. Parser p -> Parser p
lexeme p = p <* skipSpace

validNameString :: Parser String
validNameString =
  flattenChars
  <$> many1 (noneOf ['=', ' ', '<', '>', '/', '"'])

attribute :: Parser Attribute
attribute = lexeme do
  name <- validNameString
  value <- (flattenChars <$> getValue) <|> pure ""
  pure $ Attribute (Name name) (Value value)
  where
    termini = ['"', '>', ' ']
    getValue = do
      _ <- char '='
      content <- withQuotes <|> withoutQuotes
      pure content
    withQuotes = do
      _ <- char '"'
      manyTill anyChar $ void (char '"') <|> eof
    withoutQuotes = do
      content <- many $ satisfy (not flip elem ['>', ' '])
      _ <- void (char ' ') <|> eof <|> pure unit
      pure content

tagOpenOrSingleOrClose :: Parser Tag
tagOpenOrSingleOrClose = lexeme $
  char '<' *> (closeTag <|> tagOpenOrSingle)

closeTag :: Parser Tag
closeTag = lexeme do
  _ <- char '/'
  name <- validNameString
  _ <- char '>'
  pure $ TagClose (TagName name)

tagOpenOrSingle :: Parser Tag
tagOpenOrSingle = lexeme do
  tagName <- lexeme $ TagName <$> validNameString
  attrs <- many attribute <|> pure mempty
  let spec' = spec tagName attrs
  closeTagOpen spec'
    <|> closeTagSingle spec'
    <|> fail "no closure in sight for tag opening"
  where
    spec tagName attrs constructor =
      constructor tagName attrs
    closeTagOpen f =
      char '>' *> pure (f TagOpen)
    closeTagSingle f =
      string "/>" *> pure (f TagSingle)

tnode :: Parser Tag
tnode = lexeme do
  TNode <<< flattenChars <$> many1 (satisfy ((/=) '<'))

scriptTag :: Parser Tag
scriptTag = lexeme do
  _ <- lexeme $ string "<script"
  attrs <- manyTill attribute (char '>')
  content <- manyTill anyChar (string "</script>")
  pure $ TScript attrs $ flattenChars content

tag :: Parser Tag
tag = lexeme do
  scriptTag <|> tagOpenOrSingleOrClose <|> tnode

tags :: Parser (List Tag)
tags = do
  skipSpace
  many tag

parse :: forall a. Parser a -> String -> Either ParseError a
parse p s = runParser p s

parseTags :: String -> Either ParseError (List Tag)
parseTags s = parse tags s
