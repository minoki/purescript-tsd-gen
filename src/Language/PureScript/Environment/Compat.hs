module Language.PureScript.Environment.Compat
  ( stripRole
  ) where

stripRole :: [(a,b,r)] -> [(a,b)]
stripRole = map (\(a,b,_) -> (a,b))
