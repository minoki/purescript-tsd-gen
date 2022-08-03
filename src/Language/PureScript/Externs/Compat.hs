{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Language.PureScript.Externs.Compat
  ( module Language.PureScript.Externs
  , pattern EDKind
  , edKindName
  ) where

import           Language.PureScript.Externs
import           Language.PureScript.Names

pattern EDKind :: ProperName 'TypeName -> ExternsDeclaration
pattern EDKind { edKindName } <- (const Nothing -> Just edKindName)
