module Test.Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(Right, Left), either)
import Data.List (List(..), (:))
import Data.Monoid (mempty)
import LenientHtmlParser (parse, tnode, tag, attribute, TagName(TagName), Tag(TagOpen, TNode, TagClose, TagSingle), Name(Name), Value(Value), Attribute(Attribute), parseTags)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Parsing.StringParser (Parser)

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
  ((TagOpen (TagName "!DOCTYPE") ((Attribute (Name "html") (Value "")) : Nil)) : (TagOpen (TagName "table") Nil) : (TagOpen (TagName "tr") Nil) : (TagOpen (TagName "td") Nil) : (TNode "Trash") : (TagClose (TagName "td")) : (TagOpen (TagName "td") ((Attribute (Name "class") (Value "target")) : Nil)) : (TagOpen (TagName "a") ((Attribute (Name "href") (Value "http://mylink")) : Nil)) : (TNode "[悪因悪果] 今季のゴミ - 01 [140p].avi") : (TagClose (TagName "a")) : (TagClose (TagName "td")) : (TagClose (TagName "tr")) : (TagClose (TagName "table")) : Nil)


testMultiCommentHtml :: String
testMultiCommentHtml = """
<!DOCTYPE html>
<!-- whatever -->
<!-- whatever -->
<!-- whatever -->
<!-- whatever -->
<div>
</div>
"""

expectedMultiCommentTestTags :: List Tag
expectedMultiCommentTestTags =
  ((TagOpen (TagName "!DOCTYPE") ((Attribute (Name "html") (Value "")) : Nil)) : (TagOpen (TagName "div") Nil) : (TagClose (TagName "div")) : Nil)

expectTags :: forall e. String -> List Tag -> Aff e Unit
expectTags str exp =
  case parseTags str of
    Right x -> do
      assert "this should work" $ x == exp
    Left e -> do
      failure (show e)

testParser :: forall e a. (Show a, Eq a) =>
  Parser a ->
  String ->
  a ->
  Aff (console :: CONSOLE | e) Unit
testParser p s expected =
  case parse p s of
    Right x -> do
      assert "parsing worked:" $ x == expected
    Left e ->
      failure $ "parsing failed: " <> show e

main :: forall e.
  Eff
    ( "console" :: CONSOLE
    , "testOutput" :: TESTOUTPUT
    , "avar" :: AVAR
    , "fs" :: FS
    | e
    )
    Unit
main = runTest do
  suite "LenientHtmlParser" do
    test "tnode" $
      testParser tnode "a b c " $ TNode "a b c"
    test "attribute" $
      testParser attribute "abc=\"1223\"" $ Attribute (Name "abc") (Value "1223")
    test "empty attribute" $
      testParser attribute "abc=\"\"" $ Attribute (Name "abc") (Value "")
    test "tag close" $
      testParser tag "</crap>" $ TagClose (TagName "crap")
    test "tag single" $
      testParser tag "<crap/>" $ TagSingle (TagName "crap") mempty
    test "tag open" $
      testParser tag "<crap> " $ TagOpen (TagName "crap") mempty
    test "tag open with attr" $
      testParser tag "<crap a=\"sdf\"> " $ TagOpen (TagName "crap") (pure (Attribute (Name "a") (Value "sdf")))
    test "parseTags" do
      expectTags testHtml expectedTestTags
    test "multiple comments" do
      expectTags testMultiCommentHtml expectedMultiCommentTestTags
    test "test fixtures/crap.html" do
      a <- readTextFile UTF8 "fixtures/crap.html"
      either (failure <<< show) (const success) (parseTags a)
