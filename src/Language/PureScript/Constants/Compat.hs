{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.PureScript.Constants.Compat
  ( module C
#if MIN_VERSION_purescript(0, 15, 7)
  , module Language.PureScript.Constants.Compat
#endif
  ) where
import           Language.PureScript.Constants.Prim as C

#if MIN_VERSION_purescript(0, 15, 7)
import           Language.PureScript.Names (ModuleName)

pattern Prim :: ModuleName
pattern Prim = C.M_Prim
#endif
