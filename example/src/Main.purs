module Main where

import Prelude (Unit, show, (#), ($), (+))
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))
import Data.Variant (Variant, case_, on)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)
import Effect.Uncurried (EffectFn2, mkEffectFn2)

main :: Effect Unit
main = do
  log "Hello sailor!"

variantToString :: Variant (num :: Number, str :: String) -> String
variantToString = case_ # on (Proxy :: Proxy "num") show
                        # on (Proxy :: Proxy "str") (\x -> x)

nullableToString :: Nullable String -> String
nullableToString x = case toMaybe x of
  Just y -> y
  Nothing -> "null"

numToSomeObj :: Number -> Object Number
numToSomeObj x = fromFoldable [Tuple "foo" x, Tuple "bar" (x + 1.0)]

someEffectFn :: EffectFn2 Number Number Unit
someEffectFn = mkEffectFn2 $ \a b -> do
  log (show (a + b))

-- Function s t
-- Array t
-- Record { key1 :: Type1, key2 :: Type2 }
-- Number, Int
-- String, Char
-- Boolean
-- Tuple a b
-- Maybe a
-- Either a b
-- Data.Function.Uncurried
-- Effect
-- Control.Monad.Eff
-- Data.StrMap.StrMap
-- Data.Variant
-- Data.Nullable
