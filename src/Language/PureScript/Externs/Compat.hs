{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

#if MIN_VERSION_purescript(0, 14, 0)

module Language.PureScript.Externs.Compat
  ( module Language.PureScript.Externs
  , pattern EDKind
  , edKindName
  ) where

import           Language.PureScript.Externs
import           Language.PureScript.Names

pattern EDKind :: ProperName 'TypeName -> ExternsDeclaration
pattern EDKind { edKindName } <- (const Nothing -> Just edKindName)

#else

module Language.PureScript.Externs.Compat
  ( module Language.PureScript.Externs
  ) where

import           Language.PureScript.Externs

#endif
