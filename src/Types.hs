module Types (Encryptable, Decryptable, encryptable, decryptable, extractEncryptable, extractDecryptable) where

import Data.Char

newtype Encryptable = Encryptable String deriving (Show, Eq)

newtype Decryptable = Decryptable String deriving (Show, Eq)

encryptable :: String -> Maybe Encryptable
encryptable s =
  if all isAscii s
    then Just $ Encryptable s
    else Nothing

decryptable :: String -> Maybe Decryptable
decryptable s =
  if all isAscii s
    then Just $ Decryptable s
    else Nothing

extractEncryptable :: Encryptable -> String
extractEncryptable (Encryptable x) = x

extractDecryptable :: Decryptable -> String
extractDecryptable (Decryptable x) = x
