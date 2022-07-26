{-# LANGUAGE CPP #-}
module Language.PureScript.Constants.Compat
  ( module C
  ) where
#if defined(MIN_VERSION_purescript_ast) || defined(MIN_VERSION_purescript_cst) || MIN_VERSION_purescript(0, 15, 0)
import           Language.PureScript.Constants.Prim as C
#else
import           Language.PureScript.Constants as C
#endif
