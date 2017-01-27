module LenientHtmlParser where

import Prelude
import Data.Generic.Rep as Rep
import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Generic (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, toUnfoldable)
import Data.Monoid (mempty)
import Data.String (trim, fromCharArray)
import Text.Parsing.StringParser (Parser, ParseError, runParser, fail)
import Text.Parsing.StringParser.Combinators (fix, many, many1, manyTill)
import Text.Parsing.StringParser.String (anyChar, char, noneOf, satisfy, string)

newtype TagName = TagName String
derive instance eqTagName :: Eq TagName
derive instance genericTagName :: Generic TagName
derive instance genericRepTagName :: Rep.Generic TagName _
instance showTagName :: Show TagName where show = genericShow

type Attributes = List Attribute

newtype Name = Name String
derive instance eqName :: Eq Name
derive instance genericName :: Generic Name
derive instance genericRepName :: Rep.Generic Name _
instance showName :: Show Name where show = genericShow

newtype Value = Value String
derive instance eqValue :: Eq Value
derive instance genericValue :: Generic Value
derive instance genericRepValue :: Rep.Generic Value _
instance showValue :: Show Value where show = genericShow

data Attribute = Attribute Name Value
derive instance eqAttribute :: Eq Attribute
derive instance genericAttribute :: Generic Attribute
derive instance genericRepAttribute :: Rep.Generic Attribute _
instance showAttribute :: Show Attribute where show = genericShow

data Tag
  = TagOpen TagName Attributes
  | TagSingle TagName Attributes
  | TagClose TagName
  | TNode String
derive instance eqTag :: Eq Tag
derive instance genericTag :: Generic Tag
derive instance genericRepTag :: Rep.Generic Tag _
instance showTag :: Show Tag where show = genericShow

flattenChars :: List Char -> String
flattenChars = fromCharArray <<< toUnfoldable

comment :: Parser Unit
comment =
  string "<!--" *> innerComment
  where
    innerComment = fix \_ -> do
      (manyTill anyChar (string "-->") *> pure unit)
      <|> innerComment

skipSpace :: Parser Unit
skipSpace = fix \_ ->
  (comment *> skipSpace')
  <|> (many1 ws *> skipSpace')
  <|> pure unit
  where
    ws = satisfy\c ->
      c == '\n' ||
      c == '\r' ||
      c == '\t' ||
      c == ' '

skipSpace' :: Parser Unit
skipSpace' = fix \_ ->
  skipSpace

lexeme :: forall p. Parser p -> Parser p
lexeme p = p <* skipSpace

validNameString :: Parser String
validNameString =
  trim <<< flattenChars
  <$> (many1 $ noneOf ['=', ' ', '<', '>', '/', '"'])

attribute :: Parser Attribute
attribute = lexeme $ do
  name <- validNameString
  value <- getValue <|> pure ""
  pure $ Attribute (Name name) (Value value)
  where
    getValue = do
      void $ string "=\""
      value <- trim <<< flattenChars <$> (many $ noneOf ['"'])
      void $ char '"'
      pure $ value

tagOpenOrSingleOrClose :: Parser Tag
tagOpenOrSingleOrClose = lexeme $
  char '<'
  *> (closeTag <|> tagOpenOrSingle)

closeTag :: Parser Tag
closeTag = lexeme $ do
  void $ char '/'
  name <- validNameString
  void $ char '>'
  pure $ TagClose (TagName name)

tagOpenOrSingle :: Parser Tag
tagOpenOrSingle = lexeme $ do
  tagName <- TagName <$> validNameString <* skipSpace
  attrs <- many attribute <|> pure mempty
  let spec' = spec tagName attrs
  (closeTagOpen spec')
    <|> (closeTagSingle spec')
    <|> fail "no closure in sight for tag opening"
  where
    spec tagName attrs constructor =
      constructor tagName attrs
    closeTagOpen f =
      char '>' *> (pure $ f TagOpen)
    closeTagSingle f =
      string "/>" *> (pure $ f TagSingle)

tnode :: Parser Tag
tnode = lexeme $ do
  text <- flattenChars <$> (many1 $ satisfy ((/=) '<'))
  pure $ TNode (trim text)

tag :: Parser Tag
tag = lexeme do
  (tagOpenOrSingleOrClose <|> tnode)

tags :: Parser (List Tag)
tags = do
  skipSpace
  many tag

parse :: forall a. Parser a -> String -> Either ParseError a
parse p s = runParser p s

parseTags :: String -> Either ParseError (List Tag)
parseTags s = parse tags s
