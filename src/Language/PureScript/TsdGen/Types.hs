{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.PureScript.TsdGen.Types where
import Prelude hiding (elem,notElem,lookup)
import Language.PureScript.Environment
import Language.PureScript.Types
import Language.PureScript.Label
import Language.PureScript.PSString
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Kinds
import Language.PureScript.TypeChecker.Monad
import qualified Language.PureScript.Constants as C
import Language.PureScript.CodeGen.JS.Common
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Monoid ((<>))
import qualified Data.List as List
import Language.PureScript.TsdGen.Hardwired
import Language.PureScript.TsdGen.JSIdent (JSIdentifier, jsIdentToText)

data Field = Field { fieldLabel :: !Label
                   , fieldType :: !TSType
                   , fieldIsOptional :: !Bool
                   -- Other options: readonly
                   }
           | NewSignature {- type parameters -} [Text] [TSType] TSType
           deriving (Eq,Show)

mkField :: Label -> TSType -> Field
mkField label ty = Field label ty False

mkOptionalField :: Label -> TSType -> Field
mkOptionalField label ty = Field label ty True

-- TypeScript types
data TSType = TSAny
            | TSUndefined
            | TSNull
            | TSNever
            | TSNumber
            | TSBoolean
            | TSString
            | TSFunction {- type parameters -} [Text] {- parameter types -} [TSType] TSType
            | TSArray TSType
            | TSRecord [Field]
            | TSStrMap TSType -- Data.StrMap.StrMap <=> {[_: string]: T}
            | TSTyVar Text
            | TSNamed {- module id -} (Maybe JSIdentifier) {- name -} Text {- arguments -} [TSType]
            | TSStringLit PSString
            | TSUnion [TSType] -- empty = never
            | TSIntersection [TSType] -- empty = {} (all)
            | TSUnknown Text
            | TSCommented TSType Text
            deriving (Eq,Show)

constraintToType :: SourceConstraint -> SourceType
constraintToType ct = foldl (TypeApp nullSourceAnn) (TypeConstructor nullSourceAnn qDictTypeName) (constraintArgs ct)
  where qDictTypeName = fmap coerceProperName (constraintClass ct)

data TypeTranslationContext f = TypeTranslationContext { ttcBoundTyVars :: [Text]
                                                       , ttcUnboundTyVars :: [Text]
                                                       , ttcScopedVarKinds :: Maybe [(Text,SourceKind)]
                                                       , ttcGetModuleId :: ModuleName -> f (Maybe JSIdentifier)
                                                       , ttcEnvironment :: Environment
                                                       , ttcCurrentModuleName :: ModuleName
                                                       }

type TypeTranslationT f = ReaderT (TypeTranslationContext f) (ExceptT MultipleErrors f)

tsFunction :: forall f. Monad f => (SourceType -> TypeTranslationT f TSType) -> [SourceType] -> SourceType -> TypeTranslationT f TSType
tsFunction go args ret = do
  unbound <- asks ttcUnboundTyVars
  withReaderT (\r -> r { ttcBoundTyVars = ttcBoundTyVars r ++ unbound, ttcUnboundTyVars = [] })
    $ TSFunction unbound <$> traverse go args <*> go ret

pursTypeToTSType :: forall f. Monad f => SourceType -> TypeTranslationT f TSType
pursTypeToTSType = go
  where
    go :: SourceType -> TypeTranslationT f TSType
    go (TypeApp _ (TypeApp _ tcon a0) r)
      | tcon == tyFunction || tcon == tyEffectFn1 = tsFunction go [a0] r
      | tcon == tyEff = tsFunction go [] r
    go (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) r)
      | tcon == tyFn2 || tcon == tyEffectFn2 = tsFunction go [a0,a1] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) r)
      | tcon == tyFn3 || tcon == tyEffectFn3 = tsFunction go [a0,a1,a2] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) a3) r)
      | tcon == tyFn4 || tcon == tyEffectFn4 = tsFunction go [a0,a1,a2,a3] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) a3) a4) r)
      | tcon == tyFn5 || tcon == tyEffectFn5 = tsFunction go [a0,a1,a2,a3,a4] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) a3) a4) a5) r)
      | tcon == tyFn6 || tcon == tyEffectFn6 = tsFunction go [a0,a1,a2,a3,a4,a5] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) a3) a4) a5) a6) r)
      | tcon == tyFn7 || tcon == tyEffectFn7 = tsFunction go [a0,a1,a2,a3,a4,a5,a6] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) a3) a4) a5) a6) a7) r)
      | tcon == tyFn8 || tcon == tyEffectFn8 = tsFunction go [a0,a1,a2,a3,a4,a5,a6,a7] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) a3) a4) a5) a6) a7) a8) r)
      | tcon == tyFn9 || tcon == tyEffectFn9 = tsFunction go [a0,a1,a2,a3,a4,a5,a6,a7,a8] r
    go (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ (TypeApp _ tcon a0) a1) a2) a3) a4) a5) a6) a7) a8) a9) r)
      | tcon == tyFn10 || tcon == tyEffectFn10 = tsFunction go [a0,a1,a2,a3,a4,a5,a6,a7,a8,a9] r
    go (TypeApp _ tcon a0)
      | tcon == tyArray = TSArray <$> go a0
      | tcon == tyStrMap || tcon == tyForeignObject = TSStrMap <$> go a0
      | tcon == tyRecord = case rowToList a0 of
          (pairs, _) -> TSRecord <$> traverse (\(RowListItem { rowListLabel = label, rowListType = ty }) -> mkField label <$> go ty) pairs
      | tcon == tyFn0 = tsFunction go [] a0
      | tcon == tyEffect = tsFunction go [] a0
      | tcon == tyVariant = case rowToList a0 of
          (pairs, _) -> TSUnion <$> traverse (\(RowListItem { rowListLabel = label, rowListType = ty }) -> (\ty' -> TSRecord [mkField "type" (TSStringLit $ runLabel label), mkField "value" ty']) <$> go ty) pairs
      | tcon == tyNullable = (\ty -> TSUnion [ty, TSNull]) <$> go a0
    go ty@(ForAll _ name _kind inner _) = getKindsIn ty $ \kinds ->
      if List.lookup name kinds == Just kindType
      then withReaderT (\r -> r { ttcUnboundTyVars = name : ttcUnboundTyVars r }) (go inner)
      else go inner
    go (TypeVar _ name) = do
        isBound <- asks (\r -> List.elem name (ttcBoundTyVars r))
        if isBound
          then pure (TSTyVar name)
          else pure (TSUnknown $ T.pack $ "type variable " ++ T.unpack name)
    go ty@(TypeConstructor _ _qName)
      | ty == tyString = pure TSString
      | ty == tyChar = pure TSString
      | ty == tyNumber = pure TSNumber
      | ty == tyInt = pure TSNumber
      | ty == tyBoolean = pure TSBoolean
    go ty@(TypeApp _ s t) = do
      s' <- go s
      t' <- go t
      case s' of
        TSNamed m n a -> pure (TSNamed m n (a ++ [t']))
        _ -> pure (TSUnknown $ T.pack $ show ty)
    go ty@(TypeConstructor _ (Qualified (Just (ModuleName [ProperName prim])) typeName)) | prim == C.prim = do
      case typeName of
        ProperName "Partial" -> pure (TSUnknown "Prim.Partial")
        _ -> pure (TSUnknown $ T.pack $ show ty)
    go ty@(TypeConstructor _ qName@(Qualified (Just moduleName) typeName)) = do
      ti <- asks (Map.lookup qName . types . ttcEnvironment)
      case ti of
        Just (k, _) | isSimpleKind k -> do
          getModuleId <- asks ttcGetModuleId
          moduleId <- lift (lift (getModuleId moduleName))
          pure (TSNamed moduleId (properToJs typeName) [])
        _ -> pure (TSUnknown $ T.pack $ show ty)
    go (ConstrainedType _ ct inner) = tsFunction go [constraintToType ct] inner
    go ty = pure (TSUnknown $ T.pack $ show ty)

    getKindsIn :: SourceType -> ([(Text,SourceKind)] -> TypeTranslationT f r) -> TypeTranslationT f r
    getKindsIn ty m = do
      mkinds <- asks ttcScopedVarKinds
      case mkinds of
        Just kinds -> m kinds
        Nothing -> do
          checkState <- asks (\TypeTranslationContext{..} ->
                                let insertLocalTyVar env v = Map.insert (Qualified (Just ttcCurrentModuleName) (ProperName v)) (kindType, LocalTypeVariable) env
                                    env' = ttcEnvironment { types = foldl insertLocalTyVar (types ttcEnvironment) ttcBoundTyVars }
                                in (emptyCheckState env') { checkCurrentModule = Just ttcCurrentModuleName })
          case runExcept (evalStateT (kindOfWithScopedVars ty) checkState) of
            Left err -> throwError err
            Right (kind,kinds)
              | kind == kindType -> withReaderT (\r -> r { ttcScopedVarKinds = Just kinds }) (m kinds)
              | otherwise -> throwError (errorMessage (ExpectedType ty kind))

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
    where mid | Just m <- moduleid = jsIdentToText m <> "."
              | otherwise = ""
          ta | [] <- tyArgs = ""
               -- the space after '<' is needed to avoid parse error with types like Array<<a>(_: a) => a>
             | otherwise = "< " <> T.intercalate ", " (map showTSType tyArgs) <> " >"
  TSCommented inner desc -> showTSTypePrec prec inner <> " /* " <>  desc <> " */"

-- SimpleKind :: (Type -> )* Type
isSimpleKind :: SourceKind -> Bool
isSimpleKind k | k == kindType = True
isSimpleKind (FunKind _ s t) = s == kindType && isSimpleKind t
isSimpleKind _ = False

numberOfTypeParams :: SourceKind -> Int
numberOfTypeParams k | k == kindType = 0
numberOfTypeParams (FunKind _ s t) | s == kindType = numberOfTypeParams t + 1
numberOfTypeParams _ = 0 -- invalid

-- LessSimpleKind :: (SimpleKind -> )* Type
isLessSimpleKind :: SourceKind -> Bool
isLessSimpleKind k | k == kindType = True
isLessSimpleKind (FunKind _ s t) = isSimpleKind s && isLessSimpleKind t
isLessSimpleKind _ = False

extractTypes :: SourceKind -> [(a,Maybe SourceKind)] -> Maybe [a]
extractTypes k [] | k == kindType = return []
extractTypes (FunKind _ kind1 r) ((name,kind2):xs)
  | kind1 == kindType && (kind2 == Just kindType || kind2 == Nothing) = (name :) <$> extractTypes r xs
  | otherwise = extractTypes r xs -- ??
extractTypes _ _ = Nothing
