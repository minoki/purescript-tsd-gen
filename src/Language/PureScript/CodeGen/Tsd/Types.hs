{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.CodeGen.Tsd.Types
  ( showTSType
  , showParenIf
  , showField
  , showFunctionParameters
  , objectPropertyToString
  , showTSTypePrec
  ) where
import Language.PureScript.Label
import Language.PureScript.PSString
import Language.PureScript.CodeGen.JS.Common
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Monoid ((<>), mconcat)
import Data.List (intersperse)
import Language.PureScript.TsdGen.Types
import qualified Language.PureScript.CodeGen.Tsd.Identifier as JS (identToBuilder)

intercalateTB :: TB.Builder -> [TB.Builder] -> TB.Builder
intercalateTB sep xs = mconcat (intersperse sep xs)

showTSType :: TSType -> TB.Builder
showTSType = showTSTypePrec 0

showParenIf :: Bool -> TB.Builder -> TB.Builder
showParenIf True s = "(" <> s <> ")"
showParenIf False s = s

showField :: Field -> TB.Builder
showField field@Field{} = objectPropertyToString (runLabel (fieldLabel field)) <> optionalMarker <> ": " <> showTSType (fieldType field)
  where optionalMarker | fieldIsOptional field = "?"
                       | otherwise = mempty
showField (NewSignature [] params result) = "new (" <> showFunctionParameters params <> "): " <> showTSType result
showField (NewSignature tp params result) = "new <" <> intercalateTB ", " (map (TB.fromText . anyNameToJs) tp) <> ">(" <> showFunctionParameters params <> "): " <> showTSType result

showFunctionParameters :: [TSType] -> TB.Builder
showFunctionParameters [] = ""
showFunctionParameters [ty] = "_: " <> showTSType ty
showFunctionParameters types = intercalateTB ", " $ zipWith (\n ty -> "_" <> TB.decimal (n :: Int) <> ": " <> showTSType ty) [0..] types

-- |
-- >>> objectPropertyToString "hello"
-- "hello"
-- >>> objectPropertyToString "foo'"
-- "\"foo'\""
-- >>> objectPropertyToString "0"
-- "\"0\""
-- >>> objectPropertyToString "for"
-- "\"for\""
objectPropertyToString :: PSString -> TB.Builder
objectPropertyToString ps = case decodeString ps of
                              Just t | isValidJsIdentifier t -> TB.fromText t
                              _ -> TB.fromText (prettyPrintStringJS ps)

showTSTypePrec :: Int -> TSType -> TB.Builder
showTSTypePrec prec ty = case ty of
  TSAny -> "any"
  TSUndefined -> "undefined"
  TSNull -> "null"
  TSNever -> "never"
  TSNumber -> "number"
  TSBoolean -> "boolean"
  TSString -> "string"
  TSFunction [] params ret -> showParenIf (prec > 0) $ "(" <> showFunctionParameters params <> ") => " <> showTSType ret
  TSFunction tp params ret -> showParenIf (prec > 0) $ "<" <> intercalateTB ", " (map (TB.fromText . anyNameToJs) tp) <> ">(" <> showFunctionParameters params <> ") => " <> showTSType ret
  TSArray elemTy -> "Array< " <> showTSType elemTy <> " >" -- TODO: Use ReadonlyArray?
  TSStrMap elemTy -> "{[_: string]: " <> showTSType elemTy <> "}"
  TSRecord [] -> "{}"
  TSRecord fields -> "{ " <> intercalateTB "; " (map showField fields) <> " }"
  TSUnknown desc -> "any /* " <> TB.fromText desc <> " */"
  TSStringLit s -> TB.fromText (prettyPrintStringJS s)
  TSUnion [] -> "never" -- uninhabitated type
  TSUnion members -> showParenIf (prec > 1) $ intercalateTB " | " (map (showTSTypePrec 1) members)
  TSIntersection [] -> "{}" -- universal type.  TODO: use 'unknown' type?
  TSIntersection members -> intercalateTB " & " (map (showTSTypePrec 2) members)
  TSTyVar name -> TB.fromText (anyNameToJs name)
  TSNamed moduleid name tyArgs -> mid <> JS.identToBuilder name <> ta
    where mid = case moduleid of
                  Just m -> JS.identToBuilder m <> "."
                  _ -> mempty
          ta = case tyArgs of
                 [] -> mempty
                      -- the space after '<' is needed to avoid parse error with types like Array<<a>(_: a) => a>
                 _ -> "< " <> intercalateTB ", " (map showTSType tyArgs) <> " >"
  TSCommented inner desc -> showTSTypePrec prec inner <> " /* " <> TB.fromText desc <> " */"
