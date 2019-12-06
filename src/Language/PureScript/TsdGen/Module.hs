{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Language.PureScript.TsdGen.Module where
import Prelude hiding (elem,notElem,lookup)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as JSON
import Control.Arrow (first,second)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS.Strict
import System.FilePath ((</>))
import Language.PureScript.Externs
import Language.PureScript.Environment
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Label
import Language.PureScript.Errors
import Language.PureScript.PSString
import Language.PureScript.Pretty.Kinds
import qualified Language.PureScript.Constants as C
import Language.PureScript.TsdGen.Types
import Language.PureScript.TsdGen.Hardwired
import qualified Language.PureScript.CodeGen.Tsd.Identifier as JS
import Data.Version (showVersion)
import Paths_purescript_tsd_gen (version)

data ModuleProcessingError = FileReadError
                           | JSONDecodeError String FilePath
                           | PursTypeError ModuleName MultipleErrors
                           deriving (Show)

newtype ModuleImport = ModuleImport { moduleImportIdent :: Maybe JS.Identifier
                                    }

type ModuleImportMap = Map.Map ModuleName ModuleImport

type ModuleWriter = RWST () TB.Builder ModuleImportMap (ExceptT ModuleProcessingError IO)

readExternsForModule :: FilePath -> ModuleName -> ExceptT ModuleProcessingError IO ExternsFile
readExternsForModule dir moduleName = do
  let moduleNameText = T.unpack (runModuleName moduleName)
      externsPath = dir </> moduleNameText </> "externs.json"
  s <- liftIO $ BS.readFile externsPath
  case JSON.eitherDecode s of
    Left err -> throwError (JSONDecodeError err externsPath)
    Right result -> return result

recursivelyLoadExterns :: FilePath -> ModuleName -> StateT (Environment, Map.Map ModuleName (Maybe ExternsFile)) (ExceptT ModuleProcessingError IO) ()
recursivelyLoadExterns dir moduleName
  | moduleName == ModuleName [ProperName C.prim] = return () -- ~v0.11.7
  | moduleName `List.elem` C.primModules = return () -- v0.12.0~
  | otherwise = do
  ef <- lift (readExternsForModule dir moduleName)
  modify (second (Map.insert moduleName (Just ef)))
  let imports = efImports ef
  forM_ (map eiModule imports) $ \importModuleName -> do
    alreadyLoading <- gets (Map.member importModuleName . snd)
    unless alreadyLoading $ do
      modify (second (Map.insert importModuleName Nothing))
      recursivelyLoadExterns dir importModuleName
  modify (first (applyExternsFileToEnvironment ef))

emitComment :: Text -> ModuleWriter ()
emitComment t = tell ("// " <> TB.fromText t <> "\n")

emitInterface :: JS.Identifier -> [Text] -> [Field] -> ModuleWriter ()
emitInterface name tyParams fields = do
  let tyParamsText | null tyParams = mempty
                   | otherwise = "<" <> TB.fromText (T.intercalate ", " tyParams) <> ">"
  tell $ "interface " <> TB.fromText (JS.identToText name) <> tyParamsText <> " {\n" <> TB.fromText (T.concat (map (\f -> "    " <> showField f <> ";\n") fields)) <> "}\n"

emitTypeDeclaration :: Maybe Text -> JS.Identifier -> [Text] -> TSType -> ModuleWriter ()
emitTypeDeclaration comment name tyParams ty = do
  let commentPart = case comment of
                 Just commentText -> "/*" <> TB.fromText commentText <> "*/ "
                 Nothing -> mempty
  let tyParamsText | null tyParams = mempty
                   | otherwise = "<" <> TB.fromText (T.intercalate ", " tyParams) <> ">"
  tell $ "export type " <> commentPart <> TB.fromText (JS.identToText name) <> tyParamsText <> " = " <> TB.fromText (showTSType ty) <> ";\n"

data ValueExportName = NeedsRenaming { exportedName :: JS.IdentifierName, internalName :: JS.Identifier }
                     | NoRenaming JS.Identifier
                     | NotExpressibleInJSModule Text

psNameToJSValueExportName :: Text -> ValueExportName
psNameToJSValueExportName psName
  = case JS.ensureIdentifierName psName of
      Nothing -> NotExpressibleInJSModule psName -- may contain a prime symbol
      Just identifierName -> case JS.ensureNonKeyword identifierName of
                               Nothing -> NeedsRenaming { exportedName = identifierName, internalName = JS.anyNameToJs psName }
                               Just identifier -> NoRenaming identifier

emitValueDeclaration :: Maybe Text -> ValueExportName -> TSType -> ModuleWriter ()
emitValueDeclaration comment vname ty = case vname of
  NeedsRenaming { exportedName, internalName } -> do
    let intName = TB.fromText (JS.identToText internalName)
        extName = TB.fromText (JS.identToText exportedName)
    tell $ "declare const " <> intName <> ": " <> TB.fromText (showTSType ty) <> ";\n\
           \export " <> commentPart <> "{ " <> intName <> " as " <> extName <> " };\n"
  NoRenaming name -> do
    tell $ "export const " <> commentPart <> TB.fromText (JS.identToText name) <> ": " <> TB.fromText (showTSType ty) <> ";\n"
  NotExpressibleInJSModule name -> do
    -- As of PureScript 0.13.5, the compiler emits symbols that contain prime symbol `'`;
    -- Such identifiers cannot be used in ES6 modules.
    -- See: https://github.com/purescript/purescript/issues/2558
    --      https://github.com/purescript/purescript/issues/3613
    tell $ "// The identifier \"" <> TB.fromText name <> "\" cannot be expressed in JavaScript:\n\
           \// export const " <> commentPart <> TB.fromText name <> ": " <> TB.fromText (showTSType ty) <> ";\n"
  where commentPart = case comment of
                        Just commentText -> "/*" <> TB.fromText commentText <> "*/ "
                        Nothing -> mempty

emitNamespaceImport :: Monad m => JS.Identifier -> ModuleName -> WriterT TB.Builder m ()
emitNamespaceImport ident moduleName = tell $ "import * as " <> TB.fromText (JS.identToText ident) <> " from \"../" <> TB.fromText (runModuleName moduleName) <> "\";\n"

emitImport :: Monad m => ModuleName -> WriterT TB.Builder m ()
emitImport moduleName = tell $ "import \"../" <> TB.fromText (runModuleName moduleName) <> "\";\n"

processLoadedModule :: Environment -> ExternsFile -> Bool -> ExceptT ModuleProcessingError IO TB.Builder
processLoadedModule env ef importAll = execWriterT $ do
  tell $ "// module " <> TB.fromText (runModuleName currentModuleName) <> ", generated by purescript-tsd-gen " <> TB.fromString (showVersion version) <> "\n"
  (moduleImportMap, moduleBody) <- lift (execRWST (mapM_ processDecl (efDeclarations ef)) () (Map.singleton currentModuleName (ModuleImport { moduleImportIdent = Nothing })))
  if importAll
    then do
    -- Emit 'import' statements for all modules referenced, whether or not they are actually used in the type declarations.
    let explicitlyImported = List.nub (map eiModule (efImports ef))
        allImports = Map.keys moduleImportMap
    forM_ (explicitlyImported `List.union` allImports) $
      \moduleName ->
        case Map.lookup moduleName moduleImportMap of
          Just (ModuleImport { moduleImportIdent = Just ident }) -> emitNamespaceImport ident moduleName
          Nothing | moduleName /= ModuleName [ProperName C.prim] ->
            emitImport moduleName
          _ -> return ()
    else
    -- Only emit 'import' statements for modules that are actually used in the type declarations.
    forM_ (Map.toList moduleImportMap) $
         \m -> case m of
           (moduleName, ModuleImport { moduleImportIdent = Just ident }) -> emitNamespaceImport ident moduleName
           _ -> return ()
  tell moduleBody

  -- TODO: module re-exports: dig efExports / ReExportRef

  where
    currentModuleName :: ModuleName
    currentModuleName = efModuleName ef

    qualCurrentModule :: a -> Qualified a
    qualCurrentModule = Qualified (Just currentModuleName)

    -- Get the JS identifier for given module
    getModuleId :: ModuleName -> ModuleWriter (Maybe JS.Identifier)
    getModuleId (ModuleName [ProperName prim]) | prim == C.prim = return Nothing -- should not occur
    getModuleId moduleName@(ModuleName components) = do
      mid <- gets (Map.lookup moduleName)
      case mid of
        Nothing -> do -- not found
          let moduleId = Just $ JS.anyNameToJs $ T.intercalate "_" (runProperName <$> components)
          -- TODO: Make sure moduleId is unique
          modify (Map.insert moduleName (ModuleImport { moduleImportIdent = moduleId }))
          return moduleId
        Just ModuleImport{..} -> return moduleImportIdent

    makeContext :: [Text] -> TypeTranslationContext ModuleWriter
    makeContext typeVariables = TypeTranslationContext typeVariables [] Nothing getModuleId env currentModuleName

    pursTypeToTSTypeX :: [Text] -> SourceType -> ModuleWriter TSType
    pursTypeToTSTypeX ctx ty = do
      e <- runExceptT $ runReaderT (pursTypeToTSType ty) (makeContext ctx)
      case e of
        Left err -> throwError (PursTypeError currentModuleName err)
        Right tsty -> return tsty

    processDecl :: ExternsDeclaration -> ModuleWriter ()
    processDecl EDType{..} = do
      let name = edTypeName
          qTypeName = qualCurrentModule edTypeName
      if isSimpleKind edTypeKind
        then case edTypeDeclarationKind of
               -- newtype declaration:
               DataType params [(ctorPName,[member])]
                 | Just (Newtype,_,_,_) <- Map.lookup (qualCurrentModule ctorPName) (dataConstructors env) -> do
                     case extractTypes edTypeKind params of
                       Just typeParameters -> do
                         member' <- pursTypeToTSTypeX typeParameters member
                         emitTypeDeclaration (Just "newtype") (JS.properToJs name) typeParameters member'
                       Nothing -> do
                         emitComment $ "newtype " <> runProperName name <> ": kind annotation was not available"

               -- data declaration:
               DataType params ctors -> do
                 case extractTypes edTypeKind params of
                   Just typeParameters -> do
                     let buildCtorType (ctorPName,members)
                         -- the data constructor is exported:
                         -- the data constructor should be defined somewhere in this module (see EDDataConstructor case),
                         -- so just reference it.
                           | qualCurrentModule ctorPName `Map.member` dataConstructors env
                           = let fv = typeParameters `List.intersect` concatMap freeTypeVariables members
                             in TSNamed Nothing (JS.identToText (JS.properToJs name) <> "$$" <> JS.identToText (JS.properToJs ctorPName)) (map TSTyVar fv)

                         -- the data constructor is not exportd (i.e. abstract):
                         -- the marker fields are non-optional, so that they cannot be implicitly casted from other types.
                           | otherwise
                           = TSRecord [ mkField "$$pursType" (TSStringLit $ mkString $ runModuleName currentModuleName <> "." <> runProperName edTypeName)
                                      , mkField "$$pursTag" (TSStringLit $ mkString $ runProperName ctorPName)
                                      , mkField "$$abstractMarker" TSNever
                                      ]
                     emitTypeDeclaration (Just "data") (JS.properToJs name) typeParameters (TSUnion $ map buildCtorType ctors)
                   Nothing -> do
                     emitComment $ "data " <> runProperName name <> ": kind annotation was not available"

               -- type synonym:
               TypeSynonym
                 | Just (synonymArguments, synonymType) <- Map.lookup qTypeName (typeSynonyms env) -> do
                     case extractTypes edTypeKind synonymArguments of
                       Just typeParameters -> do
                         tsty <- pursTypeToTSTypeX typeParameters synonymType
                         emitTypeDeclaration (Just "synonym") (JS.properToJs name) typeParameters tsty
                       Nothing -> do
                         emitComment $ "type synonym " <> runProperName name <> ": kind annotation was not available"
                 | otherwise -> emitComment ("type (synonym) " <> runProperName name <> ": " <> prettyPrintKind edTypeKind)

               -- foreign import data:
               ExternData
                 | qTypeName == qnUnit -> do
                     -- Data.Unit
                     emitTypeDeclaration (Just "builtin") (JS.anyNameToJs "Unit") [] (TSRecord [(mkOptionalField "$$pursType" (TSStringLit "Data.Unit.Unit"))])
                 | qTypeName `List.elem` builtins -> do
                     pst <- pursTypeToTSTypeX typeParameters (foldl (TypeApp nullSourceAnn) (TypeConstructor nullSourceAnn qTypeName) (map (TypeVar nullSourceAnn) typeParameters))
                     emitTypeDeclaration (Just "builtin") (JS.properToJs name) typeParameters pst
                 | otherwise -> do
                     -- Foreign type: just use 'any' type.
                     -- External '.d.ts' file needs to be supplied for better typing.
                     emitTypeDeclaration (Just "foreign") (JS.properToJs name) typeParameters (TSUnknown "foreign")
                 where builtins = [qnFn0,qnFn2,qnFn3,qnFn4,qnFn5,qnFn6,qnFn7,qnFn8,qnFn9,qnFn10,qnStrMap,qnEffect,qnNullable]
                       n = numberOfTypeParams edTypeKind
                       typeParameters = map (\i -> "a" <> T.pack (show i)) [0..n-1]

               -- others:
               LocalTypeVariable -> emitComment ("unexpected local type variable: " <> runProperName name <> " :: " <> prettyPrintKind edTypeKind)
               ScopedTypeVar -> emitComment ("unexpected scoped type variable: " <> runProperName name <> " :: " <> prettyPrintKind edTypeKind)

        else emitComment ("type " <> runProperName name <> " :: " <> prettyPrintKind edTypeKind <> " : unsupported kind")

    processDecl EDDataConstructor{..} = do
      let name = edDataCtorName
      case Map.lookup (qualCurrentModule edDataCtorTypeCtor) (types env) of
        Just (k, DataType typeParameters constructors)
          | isSimpleKind k
          , Just fieldTypes <- List.lookup edDataCtorName constructors -> do
              tsty <- pursTypeToTSTypeX [] edDataCtorType
              case edDataCtorOrigin of
                Data -> do
                  -- Data constructor for a 'data' declaration:
                  -- Emit an interface so that type refinement via 'instanceof' works.
                  let fieldTypeVars = map fst typeParameters `List.intersect` concatMap freeTypeVariables fieldTypes
                      dataCtorSubtypeName = JS.identToText (JS.properToJs edDataCtorTypeCtor) <> "$$" <> JS.identToText (JS.properToJs name)
                      dataCtorSubtype = TSNamed Nothing dataCtorSubtypeName (map TSTyVar fieldTypeVars)
                  fieldTypesTS <- mapM (pursTypeToTSTypeX fieldTypeVars) fieldTypes
                  let mkMarkerField | length constructors == 1 = mkOptionalField -- allow structural subtyping if there are only one constructor
                                    | otherwise = mkField -- nominal typing
                      makerFields = [ mkMarkerField "$$pursType" (TSStringLit (mkString $ runModuleName currentModuleName <> "." <> runProperName edDataCtorTypeCtor))
                                    , mkMarkerField "$$pursTag" (TSStringLit (mkString $ runProperName edDataCtorName))
                                    ]
                      dataFields = zipWith (\f ty -> mkField (Label $ mkString $ runIdent f) ty) edDataCtorFields fieldTypesTS
                      Just subtypeName = JS.ensureIdentifierName dataCtorSubtypeName
                      Just subtypeIdent = JS.ensureNonKeyword subtypeName
                  emitInterface subtypeIdent fieldTypeVars (makerFields <> dataFields)

                  -- The constructor function has a 'new' signature returning that interface.
                  let ctorFieldName | null edDataCtorFields = "value"
                                    | otherwise = "create"
                      ctorType = TSRecord [ mkField ctorFieldName tsty
                                          , NewSignature fieldTypeVars fieldTypesTS dataCtorSubtype
                                          ]
                  emitValueDeclaration (Just "data ctor") (psNameToJSValueExportName (runProperName name)) ctorType

                Newtype ->
                  -- Data constructor for a 'newtype' declaration:
                  -- No 'new' signature: just define a function.
                  emitValueDeclaration (Just "newtype data ctor") (psNameToJSValueExportName (runProperName name)) tsty

        Nothing -> emitComment $ "the type of an exported data constructor must be exported: " <> runProperName name
        Just (k, DataType _typeParameters _constructors) -> emitComment $ "unrecognized data constructor: " <> runProperName name <> " kind: " <> prettyPrintKind k
        _ -> emitComment $ "unrecognized data constructor: " <> runProperName name

    processDecl EDValue{..} = do
      let name = edValueName
      tsty <- pursTypeToTSTypeX [] edValueType
      emitValueDeclaration Nothing (psNameToJSValueExportName (runIdent name)) tsty

    processDecl EDInstance{..}
      | Just constraints <- edInstanceConstraints
      , Just (_synonymParams,_synonymType) <- Map.lookup qDictTypeName (typeSynonyms env) = do
          -- TODO: This code depends on the undocumented implementation-details...
          let {-synonymInstance = replaceAllTypeVars (zip (freeTypeVariables synonymType) edInstanceTypes) synonymType-}
              dictTy = foldl (TypeApp nullSourceAnn) (TypeConstructor nullSourceAnn qDictTypeName) edInstanceTypes
              desugaredInstanceType = quantify (foldr (ConstrainedType nullSourceAnn) dictTy constraints)
          instanceTy <- pursTypeToTSTypeX [] desugaredInstanceType
          emitValueDeclaration (Just "instance") (psNameToJSValueExportName (runIdent edInstanceName)) instanceTy
      | otherwise = emitComment ("invalid instance declaration '" <> runIdent edInstanceName <> "'")
      where -- name = identToJs edInstanceName :: JS.Identifier
            qDictTypeName = fmap coerceProperName edInstanceClassName :: Qualified (ProperName 'TypeName)

    processDecl EDKind{..} = do
      -- Do nothing for kind declarations: just put a comment.
      let name = runProperName edKindName
      emitComment ("kind " <> name)

    processDecl EDTypeSynonym{} = do
      -- Ignored: should be handled in EDType case.
      return ()

    processDecl EDClass{} = do
      -- Ignored: should be handled in EDType case.
      return ()
