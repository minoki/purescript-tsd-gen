module Language.PureScript.TsdGen.Module.ReadExterns where
import           Control.Monad.Except
import qualified Data.Text as T
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           System.FilePath ((</>))
import qualified Language.PureScript.Make.Monad as Make

data ModuleProcessingError = FileReadError
                           | CBORDecodeError FilePath
                           | PursTypeError ModuleName MultipleErrors
                           deriving (Show)

readExternsForModule :: FilePath -> ModuleName -> ExceptT ModuleProcessingError IO ExternsFile
readExternsForModule dir moduleName = do
  let moduleNameText = T.unpack (runModuleName moduleName)
      externsPath = dir </> moduleNameText </> "externs.cbor"
  s <- liftIO $ Make.readCborFileIO externsPath
  case s of
    Nothing      -> throwError (CBORDecodeError externsPath)
    Just externs -> return externs
