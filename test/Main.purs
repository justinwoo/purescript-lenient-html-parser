module Test.Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(Right, Left), either)
import Data.List (List(..), (:))
import Data.Monoid (mempty)
import Global.Unsafe (unsafeStringify)
import LenientHtmlParser (Attribute(Attribute), Name(Name), Tag(..), TagName(TagName), Value(Value), attribute, parse, parseTags, tag, tags, tnode)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
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
  ((TagOpen (TagName "table") Nil) : (TagOpen (TagName "tr") Nil) : (TagOpen (TagName "td") Nil) : (TNode "Trash") : (TagClose (TagName "td")) : (TagOpen (TagName "td") ((Attribute (Name "class") (Value "target")) : Nil)) : (TagOpen (TagName "a") ((Attribute (Name "href") (Value "http://mylink")) : Nil)) : (TNode "[悪因悪果] 今季のゴミ - 01 [140p].avi") : (TagClose (TagName "a")) : (TagClose (TagName "td")) : (TagClose (TagName "tr")) : (TagClose (TagName "table")) : Nil)


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

expectTags :: forall e. String -> List Tag -> Aff e Unit
expectTags str exp =
  case parseTags str of
    Right x -> do
      assert "this should work" $ x == exp
    Left e -> do
      failure (show e)

testParser :: forall e a. Show a => Eq a =>
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
    test "parseTags" do
      expectTags testHtml expectedTestTags
    test "multiple comments" do
      expectTags testMultiCommentHtml expectedMultiCommentTestTags
    test "test fixtures/crap.html" do
      a <- readTextFile UTF8 "test/fixtures/crap.html"
      either (failure <<< unsafeStringify) (const success) $ unParser tags {str: a, pos: 0}
    test "test fixtures/megacrap.html" do
      a <- readTextFile UTF8 "test/fixtures/megacrap.html"
      either (failure <<< unsafeStringify) (const success) $ unParser tags {str: a, pos: 0}
