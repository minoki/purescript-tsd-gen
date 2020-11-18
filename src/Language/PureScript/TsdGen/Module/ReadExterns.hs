{-# LANGUAGE CPP #-}
module Language.PureScript.TsdGen.Module.ReadExterns where
import           Control.Monad.Except
import qualified Data.Text as T
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           System.FilePath ((</>))
#if MIN_VERSION_purescript(0, 13, 8)
import qualified Language.PureScript.Make.Monad as Make
#else
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
#endif

data ModuleProcessingError = FileReadError
                           | JSONDecodeError String FilePath
                           | CBORDecodeError FilePath
                           | PursTypeError ModuleName MultipleErrors
                           deriving (Show)

readExternsForModule :: FilePath -> ModuleName -> ExceptT ModuleProcessingError IO ExternsFile
#if MIN_VERSION_purescript(0, 13, 8)
readExternsForModule dir moduleName = do
  let moduleNameText = T.unpack (runModuleName moduleName)
      externsPath = dir </> moduleNameText </> "externs.cbor"
  s <- liftIO $ Make.readCborFileIO externsPath
  case s of
    Nothing      -> throwError (CBORDecodeError externsPath)
    Just externs -> return externs
#else
readExternsForModule dir moduleName = do
  let moduleNameText = T.unpack (runModuleName moduleName)
      externsPath = dir </> moduleNameText </> "externs.json"
  s <- liftIO $ BS.readFile externsPath
  case JSON.eitherDecode s of
    Left err     -> throwError (JSONDecodeError err externsPath)
    Right result -> return result
#endif
