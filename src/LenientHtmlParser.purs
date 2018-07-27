module LenientHtmlParser where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Text.Parsing.StringParser (ParseError(..), Parser(..), fail, runParser)
import Text.Parsing.StringParser.CodeUnits as Parser
import Text.Parsing.StringParser.Combinators (fix, many, many1, manyTill)

newtype TagName = TagName String
derive instance eqTagName :: Eq TagName
derive instance genericRepTagName :: Generic TagName _
derive newtype instance showTagName :: Show TagName

newtype Name = Name String
derive instance eqName :: Eq Name
derive instance genericRepName :: Generic Name _
derive newtype instance showName :: Show Name

newtype Value = Value String
derive instance eqValue :: Eq Value
derive instance genericRepValue :: Generic Value _
derive newtype instance showValue :: Show Value

data Attribute = Attribute Name Value
derive instance eqAttribute :: Eq Attribute
derive instance genericRepAttribute :: Generic Attribute _
instance showAttribute :: Show Attribute where show = genericShow

type Attributes = List Attribute

data Tag
  = TagOpen TagName Attributes
  | TagSingle TagName Attributes
  | TagClose TagName
  | TNode String
  | TScript Attributes String
derive instance eqTag :: Eq Tag
derive instance genericRepTag :: Generic Tag _
instance showTag :: Show Tag where show = genericShow

comment :: Parser Unit
comment = do
  _ <- Parser.regex "<!--"
  _ <- manyTill Parser.anyChar $ Parser.regex "-->"
  pure unit

doctype :: Parser Unit
doctype = do
  _ <- Parser.regex "<!DOCTYPE" <|> Parser.regex "<!doctype"
  _ <- takeStringTill { end: ">", allowEof: true }
  pure unit

skipSpace :: Parser Unit
skipSpace = fix \_ ->
  (comment *> skipSpace)
  <|> (doctype *> skipSpace)
  <|> (many1 ws *> skipSpace)
  <|> pure unit
  where
    ws = Parser.satisfy \c ->
      c == '\n' ||
      c == '\r' ||
      c == '\t' ||
      c == ' '

lexeme :: forall p. Parser p -> Parser p
lexeme p = p <* skipSpace

validNameString :: Parser String
validNameString =
  Parser.regex "[^= <>/\\\"]+"

attribute :: Parser Attribute
attribute = lexeme do
  name <- validNameString
  value <- getValue <|> pure ""
  pure $ Attribute (Name name) (Value value)
  where
    getValue = do
      _ <- Parser.char '='
      content <- withQuotes <|> withoutQuotes
      pure content
    withQuotes = do
      _ <- Parser.char '"'
      takeStringTill { allowEof: true, end: "\"" }
    withoutQuotes = do
      content <- Parser.regex "[^> ]+"
      pure content

tagOpenOrSingleOrClose :: Parser Tag
tagOpenOrSingleOrClose = lexeme $
  Parser.char '<' *> (closeTag <|> tagOpenOrSingle)

closeTag :: Parser Tag
closeTag = lexeme do
  _ <- Parser.char '/'
  name <- validNameString
  _ <- Parser.char '>'
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
      Parser.char '>' *> pure (f TagOpen)
    closeTagSingle f =
      Parser.regex "/>" *> pure (f TagSingle)

tnode :: Parser Tag
tnode = lexeme do
  TNode <$> Parser.regex "[^<]+"

scriptTag :: Parser Tag
scriptTag = lexeme do
  _ <- lexeme $ Parser.regex "<script"
  attrs <- many attribute
  content <- invalidSelfClosing <|> normal
  pure $ TScript attrs content
  where
    invalidSelfClosing = do
      _ <- (Parser.regex "/>")
      pure ""
    normal = do
      _ <- Parser.regex ">"
      content <- takeStringTill { end: "</script>", allowEof: false } <|> pure ""
      pure content

takeStringTill ::
  { allowEof :: Boolean
  , end :: String
  }
  -> Parser String
takeStringTill { end, allowEof } = Parser \{str, pos} ->
  let
    len = SCU.length end
    idx = SCU.indexOf' (Pattern end) pos str
  in
    case idx of
      Nothing -> if allowEof
        then Right
          { result: SCU.drop pos str
          , suffix: { str, pos: SCU.length str }
          }
        else Left
          { pos
          , error: ParseError $ "Could not close with found character: " <> end
          }
      Just i -> Right
        { result: SCU.take (i - pos) (SCU.drop pos str)
        , suffix: { str, pos: i + len }
        }

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
