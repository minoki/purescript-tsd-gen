{-# LANGUAGE CPP #-}
module Language.PureScript.Pretty.Compat (prettyPrintKind) where

#if MIN_VERSION_purescript(0, 14, 0)

import Language.PureScript.Types
import Language.PureScript.Pretty.Types (prettyPrintType)
import qualified Data.Text as T

prettyPrintKind :: Type a -> T.Text
prettyPrintKind = T.pack . prettyPrintType {- maxDepth -} 3

#else

import Language.PureScript.Pretty.Kinds (prettyPrintKind)

#endif
