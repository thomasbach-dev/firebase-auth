module FirebaseAuth.JWT.Internal
  ( jsonOptionsSnakeCase
  ) where

import qualified Data.Aeson as A

import Data.Char (isLower, toLower)

jsonOptionsSnakeCase :: A.Options
jsonOptionsSnakeCase =
  A.defaultOptions { A.fieldLabelModifier = toSnakeCase  }

toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (c:cs)
  | isLower c = '_': toLower c: toSnakeCase cs
  | otherwise = c: toSnakeCase cs
