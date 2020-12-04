{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.PureScript.TsdGen.Types where
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript.CodeGen.Tsd.Identifier as JS
import qualified Language.PureScript.Constants.Compat as C
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Kinds.Compat
import           Language.PureScript.Label
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Language.PureScript.TsdGen.Hardwired
import           Language.PureScript.TypeChecker.Kinds
import           Language.PureScript.TypeChecker.Monad
import           Language.PureScript.Types
import           Prelude hiding (elem, lookup, notElem)

data Field = Field { fieldLabel      :: !Label
                   , fieldType       :: !TSType
                   , fieldIsOptional :: !Bool
                   -- Other options: readonly
                   }
           | NewSignature {- type parameters -} [Text] [TSType] TSType
           deriving (Eq,Show)

mkField :: Label -> TSType -> Field
mkField label ty = Field label ty False

mkOptionalField :: Label -> TSType -> Field
mkOptionalField label ty = Field label ty True

data TSTypeName = QualifiedTypeName {- module id -} JS.Identifier {- name -} JS.IdentifierName
                | UnqualifiedTypeName {- name -} JS.Identifier
                deriving (Eq,Show)

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
            | TSNamed {- type name -} TSTypeName {- arguments -} [TSType]
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
                                                       , ttcGetModuleId :: ModuleName -> f (Maybe JS.Identifier)
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
          else pure (TSUnknown $ "type variable " <> name)
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
        TSNamed n a -> pure (TSNamed n (a ++ [t']))
        _           -> pure (TSUnknown $ T.pack $ show ty)
    go ty@(TypeConstructor _ (Qualified (Just C.Prim) typeName)) = do
      case typeName of
        ProperName "Partial" -> pure (TSUnknown "Prim.Partial")
        _                    -> pure (TSUnknown $ T.pack $ show ty)
    go ty@(TypeConstructor _ qName@(Qualified (Just moduleName) typeName)) = do
      ti <- asks (Map.lookup qName . types . ttcEnvironment)
      case ti of
        Just (k, _) | isSimpleKind k -> do
          getModuleId <- asks ttcGetModuleId
          moduleId <- lift (lift (getModuleId moduleName))
          let tsTypeName = case moduleId of
                             Nothing -> UnqualifiedTypeName $ JS.properToJs typeName
                             Just moduleId' -> QualifiedTypeName moduleId' $
                               case JS.ensureIdentifierName (runProperName typeName) of
                                 Just identifierName -> identifierName
                                 Nothing -> JS.toIdentifierName $ JS.properToJs typeName -- may contain prime in the type name
          pure $ TSNamed tsTypeName []
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
#if MIN_VERSION_purescript(0, 14, 0)
            Right ((kinds,_ty'),kind)
#else
            Right (kind,kinds)
#endif
              | kind == kindType -> withReaderT (\r -> r { ttcScopedVarKinds = Just kinds }) (m kinds)
              | otherwise -> throwError (errorMessage (ExpectedType ty kind))

-- SimpleKind :: (Type -> )* Type
isSimpleKind :: SourceKind -> Bool
isSimpleKind k               | k == kindType = True
isSimpleKind (FunKind _ s t) = s == kindType && isSimpleKind t
isSimpleKind _               = False

numberOfTypeParams :: SourceKind -> Int
numberOfTypeParams k               | k == kindType = 0
numberOfTypeParams (FunKind _ s t) | s == kindType = numberOfTypeParams t + 1
numberOfTypeParams _               = 0 -- invalid

-- LessSimpleKind :: (SimpleKind -> )* Type
isLessSimpleKind :: SourceKind -> Bool
isLessSimpleKind k               | k == kindType = True
isLessSimpleKind (FunKind _ s t) = isSimpleKind s && isLessSimpleKind t
isLessSimpleKind _               = False

extractTypes :: SourceKind -> [(a, Maybe SourceKind)] -> Maybe [a]
extractTypes k [] | k == kindType = return []
extractTypes (FunKind _ kind1 r) ((name,kind2):xs)
  | kind1 == kindType && (kind2 == Just kindType || kind2 == Nothing) = (name :) <$> extractTypes r xs
  | otherwise = extractTypes r xs -- ??
extractTypes _ _ = Nothing
