{-# LANGUAGE CPP #-}
module Language.PureScript.Environment.Compat
  ( stripRole
  ) where

#if MIN_VERSION_purescript(0, 14, 0)

stripRole :: [(a,b,r)] -> [(a,b)]
stripRole = map (\(a,b,_) -> (a,b))

#else

stripRole :: [(a,b)] -> [(a,b)]
stripRole = id

#endif
