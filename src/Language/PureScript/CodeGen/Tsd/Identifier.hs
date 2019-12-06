{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Language.PureScript.CodeGen.Tsd.Identifier
  ( isIdentifierName
  , IncludeKeywords(..)
  , JSIdent
  , jsIdentToText
  , JSIdentifierName
  , JSIdentifier
  , identToJs
  , properToJs
  , anyNameToJs
  , ensureNonKeyword
  , ensureIdentifierName
  ) where
import Language.PureScript.Names
import qualified Language.PureScript.CodeGen.JS.Common as JSC
import qualified Data.Text as T
import Data.Char (isLetter, isAlphaNum)

-- $
-- Behaviors of JSC.identToJs, JSC.properToJs, and JSC.anyNameToJs:
-- >>> JSC.identToJs (Ident "foo")
-- "foo"
-- >>> JSC.identToJs (Ident "foo'")
-- "foo$prime"
-- >>> JSC.identToJs (Ident "for")
-- "$$for"
-- >>> JSC.properToJs (ProperName "Foo")
-- "Foo"
-- >>> JSC.properToJs (ProperName "Foo'")
-- "Foo$prime"
-- >>> JSC.anyNameToJs "foo"
-- "foo"
-- >>> JSC.anyNameToJs "foo'"
-- "foo$prime"
-- >>> JSC.anyNameToJs "for"
-- "$$for"

isIdentifierStart, isIdentifierPart :: Char -> Bool
isIdentifierStart c = isLetter c || c == '$' || c == '_' -- TODO: Match with "ID_Start"
isIdentifierPart c = isAlphaNum c || c == '$' || c == '_' -- TODO: Match with "ID_Continue"

-- |
-- prop> all isIdentifierName ["foo", "x86", "PureScript", "$foobar", "__proto__"]
-- prop> not (any isIdentifierName ["", "foo'", "42"])
isIdentifierName :: T.Text -> Bool
isIdentifierName name = case T.uncons name of
                          Just (head, tail) -> isIdentifierStart head && T.all isIdentifierPart tail
                          _ -> False

data IncludeKeywords = IncludeKeywords
                     | ExcludeKeywords

newtype JSIdent (k :: IncludeKeywords) = JSIdent T.Text
  deriving (Eq,Show)

jsIdentToText :: JSIdent k -> T.Text
jsIdentToText (JSIdent name) = name

type JSIdentifierName = JSIdent 'IncludeKeywords
type JSIdentifier = JSIdent 'ExcludeKeywords

identToJs :: Ident -> JSIdent 'ExcludeKeywords
identToJs = JSIdent . JSC.identToJs

properToJs :: ProperName a -> JSIdent 'ExcludeKeywords
properToJs = JSIdent . JSC.properToJs

anyNameToJs :: T.Text -> JSIdent 'ExcludeKeywords
anyNameToJs = JSIdent . JSC.anyNameToJs

-- |
-- >>> ensureNonKeyword (JSIdent "foo")
-- Just (JSIdent "foo")
-- >>> ensureNonKeyword (JSIdent "for")
-- Nothing
ensureNonKeyword :: JSIdent 'IncludeKeywords -> Maybe (JSIdent 'ExcludeKeywords)
ensureNonKeyword (JSIdent name) | JSC.nameIsJsReserved name = Nothing
                                | otherwise = Just (JSIdent name)

-- |
-- >>> ensureIdentifierName "foo"
-- Just (JSIdent "foo")
-- >>> ensureIdentifierName "for"
-- Just (JSIdent "for")
-- >>> ensureIdentifierName "foo'"
-- Nothing
ensureIdentifierName :: T.Text -> Maybe (JSIdent 'IncludeKeywords)
ensureIdentifierName name | isIdentifierName name = Just (JSIdent name)
                          | otherwise = Nothing
