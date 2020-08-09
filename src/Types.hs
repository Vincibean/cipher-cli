module Types (Encryptable, Decryptable, Key, encryptable, decryptable, extractEncryptable, extractDecryptable, key, extractKey) where

import Data.Char

newtype Encryptable = Encryptable String deriving (Show, Eq)

newtype Decryptable = Decryptable String deriving (Show, Eq)

newtype Key = Key String deriving (Show, Eq)

encryptable :: String -> Maybe Encryptable
encryptable s =
  if all isAscii s
    then Just $ Encryptable s
    else Nothing

extractEncryptable :: Encryptable -> String
extractEncryptable (Encryptable x) = x

decryptable :: String -> Maybe Decryptable
decryptable s =
  if all isAscii s
    then Just $ Decryptable s
    else Nothing

extractDecryptable :: Decryptable -> String
extractDecryptable (Decryptable x) = x

key :: String -> Maybe Key
key s = 
  if (all isAscii s && (not . null) s)
    then Just $ Key s
    else Nothing

extractKey :: Key -> String
extractKey (Key x) = x