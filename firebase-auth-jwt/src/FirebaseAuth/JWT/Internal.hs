module FirebaseAuth.JWT.Internal
  ( jsonOptionsSkipLowerCaseThenSnakeCase
  ) where

import qualified Data.Aeson as A

import Data.Char (isLower, toLower)

jsonOptionsSkipLowerCaseThenSnakeCase :: A.Options
jsonOptionsSkipLowerCaseThenSnakeCase =
  A.defaultOptions { A.fieldLabelModifier = toSnakeCase . lowerFirst . dropWhile isLower }

lowerFirst :: String -> String
lowerFirst (c:cs) = toLower c:cs
lowerFirst []     = []

toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (c:cs)
  | isLower c = '_': toLower c: toSnakeCase cs
  | otherwise = c: toSnakeCase cs
