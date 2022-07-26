{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Language.PureScript.TsdGen.Hardwired where
import Language.PureScript.Types (SourceType, Type(TypeConstructor))
import Language.PureScript.Names
import Language.PureScript.Errors (nullSourceAnn)

-- Data.Unit
qnUnit :: Qualified (ProperName 'TypeName)
qnUnit = mkQualified (ProperName "Unit") (moduleNameFromString "Data.Unit")

-- Data.Function.Uncurried
qnFn0, qnFn2, qnFn3, qnFn4, qnFn5, qnFn6, qnFn7, qnFn8, qnFn9, qnFn10 :: Qualified (ProperName 'TypeName)
tyFn0, tyFn2, tyFn3, tyFn4, tyFn5, tyFn6, tyFn7, tyFn8, tyFn9, tyFn10 :: SourceType
modDataFunctionUncurried :: QualifiedBy
modDataFunctionUncurried = ByModuleName (moduleNameFromString "Data.Function.Uncurried")
qnFn0 = Qualified modDataFunctionUncurried (ProperName "Fn0")
qnFn2 = Qualified modDataFunctionUncurried (ProperName "Fn2")
qnFn3 = Qualified modDataFunctionUncurried (ProperName "Fn3")
qnFn4 = Qualified modDataFunctionUncurried (ProperName "Fn4")
qnFn5 = Qualified modDataFunctionUncurried (ProperName "Fn5")
qnFn6 = Qualified modDataFunctionUncurried (ProperName "Fn6")
qnFn7 = Qualified modDataFunctionUncurried (ProperName "Fn7")
qnFn8 = Qualified modDataFunctionUncurried (ProperName "Fn8")
qnFn9 = Qualified modDataFunctionUncurried (ProperName "Fn9")
qnFn10 = Qualified modDataFunctionUncurried (ProperName "Fn10")
tyFn0 = TypeConstructor nullSourceAnn qnFn0
tyFn2 = TypeConstructor nullSourceAnn qnFn2
tyFn3 = TypeConstructor nullSourceAnn qnFn3
tyFn4 = TypeConstructor nullSourceAnn qnFn4
tyFn5 = TypeConstructor nullSourceAnn qnFn5
tyFn6 = TypeConstructor nullSourceAnn qnFn6
tyFn7 = TypeConstructor nullSourceAnn qnFn7
tyFn8 = TypeConstructor nullSourceAnn qnFn8
tyFn9 = TypeConstructor nullSourceAnn qnFn9
tyFn10 = TypeConstructor nullSourceAnn qnFn10

-- Effect.Uncurried
qnEffectFn1, qnEffectFn2, qnEffectFn3, qnEffectFn4, qnEffectFn5, qnEffectFn6, qnEffectFn7, qnEffectFn8, qnEffectFn9, qnEffectFn10 :: Qualified (ProperName 'TypeName)
tyEffectFn1, tyEffectFn2, tyEffectFn3, tyEffectFn4, tyEffectFn5, tyEffectFn6, tyEffectFn7, tyEffectFn8, tyEffectFn9, tyEffectFn10 :: SourceType
modEffectUncurried :: QualifiedBy
modEffectUncurried = ByModuleName (moduleNameFromString "Effect.Uncurried")
qnEffectFn1 = Qualified modEffectUncurried (ProperName "EffectFn1")
qnEffectFn2 = Qualified modEffectUncurried (ProperName "EffectFn2")
qnEffectFn3 = Qualified modEffectUncurried (ProperName "EffectFn3")
qnEffectFn4 = Qualified modEffectUncurried (ProperName "EffectFn4")
qnEffectFn5 = Qualified modEffectUncurried (ProperName "EffectFn5")
qnEffectFn6 = Qualified modEffectUncurried (ProperName "EffectFn6")
qnEffectFn7 = Qualified modEffectUncurried (ProperName "EffectFn7")
qnEffectFn8 = Qualified modEffectUncurried (ProperName "EffectFn8")
qnEffectFn9 = Qualified modEffectUncurried (ProperName "EffectFn9")
qnEffectFn10 = Qualified modEffectUncurried (ProperName "EffectFn10")
tyEffectFn1 = TypeConstructor nullSourceAnn qnEffectFn1
tyEffectFn2 = TypeConstructor nullSourceAnn qnEffectFn2
tyEffectFn3 = TypeConstructor nullSourceAnn qnEffectFn3
tyEffectFn4 = TypeConstructor nullSourceAnn qnEffectFn4
tyEffectFn5 = TypeConstructor nullSourceAnn qnEffectFn5
tyEffectFn6 = TypeConstructor nullSourceAnn qnEffectFn6
tyEffectFn7 = TypeConstructor nullSourceAnn qnEffectFn7
tyEffectFn8 = TypeConstructor nullSourceAnn qnEffectFn8
tyEffectFn9 = TypeConstructor nullSourceAnn qnEffectFn9
tyEffectFn10 = TypeConstructor nullSourceAnn qnEffectFn10

-- Data.StrMap (from purescript-maps)
-- foreign import data StrMap :: Type -> Type
qnStrMap :: Qualified (ProperName 'TypeName)
qnStrMap = mkQualified (ProperName "StrMap") (moduleNameFromString "Data.StrMap")
tyStrMap :: SourceType
tyStrMap = TypeConstructor nullSourceAnn qnStrMap

-- Control.Monad.Eff
-- foreign import data Eff :: # Effect -> Type -> Type
tyEff :: SourceType
tyEff = TypeConstructor nullSourceAnn (mkQualified (ProperName "Eff") (moduleNameFromString "Control.Monad.Eff"))

-- Effect (from purescript-effect)
-- foreign import data Effect :: Type -> Type
qnEffect :: Qualified (ProperName 'TypeName)
qnEffect = mkQualified (ProperName "Effect") (moduleNameFromString "Effect")
tyEffect :: SourceType
tyEffect = TypeConstructor nullSourceAnn qnEffect

-- Data.Variant (from purescript-variant)
-- foreign import data Variant :: # Type -> Type
tyVariant :: SourceType
tyVariant = TypeConstructor nullSourceAnn (mkQualified (ProperName "Variant") (moduleNameFromString "Data.Variant"))

-- Data.Nullable (from purescript-nullable)
-- foreign import data Nullable :: Type -> Type
qnNullable :: Qualified (ProperName 'TypeName)
qnNullable = mkQualified (ProperName "Nullable") (moduleNameFromString "Data.Nullable")
tyNullable :: SourceType
tyNullable = TypeConstructor nullSourceAnn qnNullable

-- Foreign.Object (from purescript-foreign-object)
-- foreign import data Object :: Type -> Type
qnForeignObject :: Qualified (ProperName 'TypeName)
qnForeignObject = mkQualified (ProperName "Object") (moduleNameFromString "Foreign.Object")
tyForeignObject :: SourceType
tyForeignObject = TypeConstructor nullSourceAnn qnForeignObject
