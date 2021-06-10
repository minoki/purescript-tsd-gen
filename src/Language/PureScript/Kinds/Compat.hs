{-# LANGUAGE CPP #-}
#if defined(MIN_VERSION_purescript_ast) || defined(MIN_VERSION_purescript_cst)
{-# LANGUAGE PatternSynonyms #-}
module Language.PureScript.Kinds.Compat
  ( SourceKind
  , pattern FunKind
  ) where
import           Language.PureScript.Types
import qualified Language.PureScript.Constants.Prim as C

type SourceKind = SourceType

pattern FunKind :: a -> Type a -> Type a -> Type a
pattern FunKind s a b <- TypeApp s (TypeApp _ (TypeConstructor _ C.Function) a) b where
  FunKind s a b = TypeApp s (TypeApp s (TypeConstructor s C.Function) a) b where

#else

module Language.PureScript.Kinds.Compat
  ( module M
  ) where
import           Language.PureScript.Kinds as M

#endif
