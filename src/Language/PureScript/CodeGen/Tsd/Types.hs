{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.CodeGen.Tsd.Types where
import Prelude hiding (elem,notElem,lookup)
import Language.PureScript.Label
import Language.PureScript.PSString
import Language.PureScript.CodeGen.JS.Common
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.List as List
import Language.PureScript.TsdGen.Types
import qualified Language.PureScript.CodeGen.Tsd.Identifier as JS (Identifier, identToText)

showTSType :: TSType -> Text
showTSType = showTSTypePrec 0

showParenIf :: Bool -> Text -> Text
showParenIf True s = "(" <> s <> ")"
showParenIf False s = s

showField :: Field -> Text
showField field@Field{} = objectPropertyToString (runLabel (fieldLabel field)) <> optionalMarker <> ": " <> showTSType (fieldType field)
  where optionalMarker | fieldIsOptional field = "?"
                       | otherwise = ""
showField (NewSignature [] params result) = "new (" <> showFunctionParameters params <> "): " <> showTSType result
showField (NewSignature tp params result) = "new <" <> T.intercalate ", " (map anyNameToJs tp) <> ">(" <> showFunctionParameters params <> "): " <> showTSType result

showFunctionParameters :: [TSType] -> Text
showFunctionParameters [] = ""
showFunctionParameters [ty] = "_: " <> showTSType ty
showFunctionParameters types = T.intercalate ", " $ zipWith (\n ty -> "_" <> T.pack (show (n :: Int)) <> ": " <> showTSType ty) [0..] types

-- |
-- >>> objectPropertyToString "hello"
-- "hello"
-- >>> objectPropertyToString "foo'"
-- "\"foo'\""
-- >>> objectPropertyToString "0"
-- "\"0\""
-- >>> objectPropertyToString "for"
-- "\"for\""
objectPropertyToString :: PSString -> Text
objectPropertyToString ps = case decodeString ps of
                              Just t | isValidJsIdentifier t -> t
                              _ -> prettyPrintStringJS ps

showTSTypePrec :: Int -> TSType -> Text
showTSTypePrec prec ty = case ty of
  TSAny -> "any"
  TSUndefined -> "undefined"
  TSNull -> "null"
  TSNever -> "never"
  TSNumber -> "number"
  TSBoolean -> "boolean"
  TSString -> "string"
  TSFunction [] params ret -> showParenIf (prec > 0) $ "(" <> showFunctionParameters params <> ") => " <> showTSType ret
  TSFunction tp params ret -> showParenIf (prec > 0) $ "<" <> T.intercalate ", " (map anyNameToJs tp) <> ">(" <> showFunctionParameters params <> ") => " <> showTSType ret
  TSArray elemTy -> "Array< " <> showTSType elemTy <> " >" -- TODO: Use ReadonlyArray?
  TSStrMap elemTy -> "{[_: string]: " <> showTSType elemTy <> "}"
  TSRecord [] -> "{}"
  TSRecord fields -> "{ " <> T.intercalate "; " (map showField fields) <> " }"
  TSUnknown desc -> "any /* " <>  desc <> " */"
  TSStringLit s -> prettyPrintStringJS s
  TSUnion [] -> "never" -- uninhabitated type
  TSUnion members -> showParenIf (prec > 1) $ T.intercalate " | " (map (showTSTypePrec 1) members)
  TSIntersection [] -> "{}" -- universal type.  TODO: use 'unknown' type?
  TSIntersection members -> T.intercalate " & " (map (showTSTypePrec 2) members)
  TSTyVar name -> anyNameToJs name
  TSNamed moduleid name tyArgs -> mid <> name <> ta
    where mid | Just m <- moduleid = JS.identToText m <> "."
              | otherwise = ""
          ta | [] <- tyArgs = ""
               -- the space after '<' is needed to avoid parse error with types like Array<<a>(_: a) => a>
             | otherwise = "< " <> T.intercalate ", " (map showTSType tyArgs) <> " >"
  TSCommented inner desc -> showTSTypePrec prec inner <> " /* " <>  desc <> " */"
