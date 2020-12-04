{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Language.PureScript.Environment.Compat
  ( pattern DataType'
  , stripRole
  ) where

#if MIN_VERSION_purescript(0, 14, 0)

import           Language.PureScript.Environment
import           Language.PureScript.Types
import           Language.PureScript.Names
import           Data.Text (Text)

stripRole :: [(a,b,r)] -> [(a,b)]
stripRole = map (\(a,b,_) -> (a,b))

pattern DataType' :: [(Text, Maybe SourceType)] -> [(ProperName 'ConstructorName, [SourceType])] -> TypeKind
pattern DataType' p c <- DataType (stripRole -> p) c

{-# COMPLETE DataType', TypeSynonym, ExternData, LocalTypeVariable, ScopedTypeVar #-}

#else

import           Language.PureScript.Environment
import           Language.PureScript.Types
import           Language.PureScript.Names
import           Language.PureScript.Kinds.Compat (SourceKind)
import           Data.Text (Text)

stripRole :: [(a,b)] -> [(a,b)]
stripRole = id

pattern DataType' :: [(Text, Maybe SourceKind)] -> [(ProperName 'ConstructorName, [SourceType])] -> TypeKind
pattern DataType' p c <- DataType p c

{-# COMPLETE DataType', TypeSynonym, ExternData, LocalTypeVariable, ScopedTypeVar #-}

#endif
