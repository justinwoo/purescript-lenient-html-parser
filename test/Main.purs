module Test.Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (find)
import Data.Array as Array
import Data.Either (Either(Right, Left))
import Data.Foldable (traverse_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log, logShow)
import LenientHtmlParser (Attribute(Attribute), Name(Name), Tag(..), TagName(TagName), Value(Value), attribute, parse, parseTags, tag, tags, tnode)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Parsing.StringParser (Parser, unParser)

testHtml :: String
testHtml = """
<!DOCTYPE html>
<!-- whatever -->
<table>
  <tr>
    <td>Trash</td>
    <td class="target">
      <a href="http://mylink">
        [悪因悪果] 今季のゴミ - 01 [140p].avi
      </a>
    </td>
  </tr>
</table>
"""

expectedTestTags :: List Tag
expectedTestTags =
  ((TagOpen (TagName "table") Nil) : (TagOpen (TagName "tr") Nil) : (TagOpen (TagName "td") Nil) : (TNode "Trash") : (TagClose (TagName "td")) : (TagOpen (TagName "td") ((Attribute (Name "class") (Value "target")) : Nil)) : (TagOpen (TagName "a") ((Attribute (Name "href") (Value "http://mylink")) : Nil)) : (TNode "[悪因悪果] 今季のゴミ - 01 [140p].avi\n      ") : (TagClose (TagName "a")) : (TagClose (TagName "td")) : (TagClose (TagName "tr")) : (TagClose (TagName "table")) : Nil)


testMultiCommentHtml :: String
testMultiCommentHtml = """
<!DOCTYPE html "-//W3C//DTD XHTML 1.0 Transitional//EN">
<!-- whatever -->
<!-- whatever -->
<!-- whatever -->
<!-- whatever -->
<div>
</div>
"""

expectedMultiCommentTestTags :: List Tag
expectedMultiCommentTestTags =
  ((TagOpen (TagName "div") Nil) : (TagClose (TagName "div")) : Nil)

expectTags :: String -> List Tag -> Aff Unit
expectTags str exp =
  case parseTags str of
    Right x -> do
      Assert.equal exp x
    Left e -> do
      failure (show e)

testParser :: forall a. Show a => Eq a =>
  Parser a ->
  String ->
  a ->
  Aff Unit
testParser p s expected =
  case parse p s of
    Right x -> do
      Assert.shouldEqual x expected
    Left e ->
      failure $ "parsing failed: " <> show e

main :: Effect Unit
main = runTest do
  suite "LenientHtmlParser" do
    test "tnode that ends with bracket" $
      testParser tnode "a b c<" $ TNode "a b c"
    test "tnode that ends without bracket" $
      testParser tnode "a b c" $ TNode "a b c"
    test "attribute" $
      testParser attribute "abc=\"1223\"" $ Attribute (Name "abc") (Value "1223")
    test "empty attribute" $
      testParser attribute "abc=\"\"" $ Attribute (Name "abc") (Value "")
    test "attribute with no quotes" $
      testParser attribute "for=autoplay-toggle-id" $ Attribute (Name "for") (Value "autoplay-toggle-id")
    test "tag close" $
      testParser tag "</crap>" $ TagClose (TagName "crap")
    test "tag single" $
      testParser tag "<crap/>" $ TagSingle (TagName "crap") mempty
    test "tag open" $
      testParser tag "<crap> " $ TagOpen (TagName "crap") mempty
    test "tag open with attr" $
      testParser tag "<crap a=\"sdf\"> " $ TagOpen (TagName "crap") (pure (Attribute (Name "a") (Value "sdf")))
    test "tag DOCTYPE" $
      testParser tags """
        <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
      """ $ mempty
    test "tag script" $
      testParser tag """<script></script>""" $
        TScript mempty ""
    test "tag script with content" $
      testParser tag """<script>相思相愛</script>""" $
        TScript mempty "相思相愛"
    test "tag script with attribute" $
      testParser tag """<script src="test"></script>""" $
        TScript (pure (Attribute (Name "src") (Value "test"))) ""
    test "tag script improper" $
      testParser tag """<script src="test" >""" $
        TScript (pure (Attribute (Name "src") (Value "test"))) ""
    test "parseTags" do
      expectTags testHtml expectedTestTags
    test "multiple comments" do
      expectTags testMultiCommentHtml expectedMultiCommentTestTags

    test "test fixtures/crap.html" do
      a <- readTextFile UTF8 "test/fixtures/crap.html"
      case unParser tags {str: a, pos: 0} of
        Left e ->
          failure $ "Failed: " <> show e
        Right tags -> do
          -- traverse_ logShow tags.result
          success

    test "test fixtures/megacrap-formatted.html" do
      a <- readTextFile UTF8 "test/fixtures/megacrap-formatted.html"
      case unParser tags {str: a, pos: 0} of
        Left e -> do
          failure $ "Failed: " <> show e <> " from around " <> (SCU.take 40 $ SCU.drop (e.pos - 40) a)
        Right tags -> do
          -- traverse_ logShow tags.result
          success

    test "test fixtures/megacrap.html" do
      a <- readTextFile UTF8 "test/fixtures/megacrap.html"
      let tags = parseTags a
      case getYTLinks <$> tags of
        Left e ->
          failure $ "Failed: " <> show e
        Right (Tuple tags' List.Nil) -> do
          traverse_ logShow tags'
          failure "Unable to find items"
        Right (Tuple _tags' xs) -> do
          traverse_ logShow xs
          success

getYTLinks :: List Tag -> Tuple (List Tag) (List String)
getYTLinks tags =
  Tuple tags $ tailRec getLinks (Tuple mempty tags)
  where
    getLinks (Tuple acc (TagOpen (TagName "a") attrs : TNode tnode : TagClose (TagName "a") : xs))
      | Just true <- S.contains (S.Pattern "yt-uix-tile-link") <$> (getAttr "class" attrs)
      , title <- S.trim tnode
      , Just (Just href) <- Array.head <<< S.split (S.Pattern "&") <$> getAttr "href" attrs
      , link <- "https://www.youtube.com" <> href = Loop (Tuple (List.Cons (title <> ": " <> link) acc) xs)
      | otherwise = Loop (Tuple acc xs)
    getLinks (Tuple acc (_ : xs)) = Loop (Tuple acc xs)
    getLinks (Tuple acc _) = Done acc
    getAttr match xs = getValue <$> find matchName xs
      where
        matchName (Attribute (Name name) _) = match == name
        getValue (Attribute _ (Value x)) = S.trim $ x
