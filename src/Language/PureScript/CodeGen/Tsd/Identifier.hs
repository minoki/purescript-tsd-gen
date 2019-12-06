{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Language.PureScript.CodeGen.Tsd.Identifier
  ( isIdentifierName
  , IncludeKeywords(..)
  , Ident
  , identToText
  , IdentifierName
  , Identifier
  , identToJs
  , properToJs
  , anyNameToJs
  , ensureNonKeyword
  , ensureIdentifierName
  ) where
import qualified Language.PureScript.Names as PS
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

newtype Ident (k :: IncludeKeywords) = Ident T.Text
  deriving (Eq,Show)

identToText :: Ident k -> T.Text
identToText (Ident name) = name

type IdentifierName = Ident 'IncludeKeywords
type Identifier = Ident 'ExcludeKeywords

identToJs :: PS.Ident -> Ident 'ExcludeKeywords
identToJs = Ident . JSC.identToJs

properToJs :: PS.ProperName a -> Ident 'ExcludeKeywords
properToJs = Ident . JSC.properToJs

anyNameToJs :: T.Text -> Ident 'ExcludeKeywords
anyNameToJs = Ident . JSC.anyNameToJs

-- |
-- >>> ensureNonKeyword (JSIdent "foo")
-- Just (JSIdent "foo")
-- >>> ensureNonKeyword (JSIdent "for")
-- Nothing
ensureNonKeyword :: Ident 'IncludeKeywords -> Maybe (Ident 'ExcludeKeywords)
ensureNonKeyword (Ident name) | JSC.nameIsJsReserved name = Nothing
                              | otherwise = Just (Ident name)

-- |
-- >>> ensureIdentifierName "foo"
-- Just (JSIdent "foo")
-- >>> ensureIdentifierName "for"
-- Just (JSIdent "for")
-- >>> ensureIdentifierName "foo'"
-- Nothing
ensureIdentifierName :: T.Text -> Maybe (Ident 'IncludeKeywords)
ensureIdentifierName name | isIdentifierName name = Just (Ident name)
                          | otherwise = Nothing
