module Language.PureScript.Pretty.Compat (prettyPrintKind) where
import Language.PureScript.Types
import Language.PureScript.Pretty.Types (prettyPrintType)
import qualified Data.Text as T

prettyPrintKind :: Type a -> T.Text
prettyPrintKind = T.pack . prettyPrintType {- maxDepth -} 3
