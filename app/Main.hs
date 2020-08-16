{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Cipher                        as C
import           Types
import           System.Console.CmdArgs
import           Data.Maybe

data Op = Encrypt | Decrypt deriving (Show, Eq, Data, Typeable)

data Algo = Caesar {op :: Op, msg :: String, numKey :: Int} | Vigenere {op :: Op, msg :: String, stringKey :: String} deriving (Show, Eq, Data, Typeable)

caesar =
  Caesar
      { numKey =
        def
          &= help
               "The integer to use as key. It can be either a positive or a negative number"
      , op     =
        enum
          [ Encrypt &= help "Encrypt a message"
          , Decrypt &= help "Decrypt a message"
          ]
      , msg    = def &= help "The message to encrypt / decrypt"
      }
    &= help "Use Caesar's encryption / decryption algorithm"

vigenere =
  Vigenere
      { stringKey = def
        &= help "The string to use as key. It must not be an empty string"
      , op        = enum
                      [ Encrypt &= help "Encrypt a message" &= name "e"
                      , Decrypt &= help "Decrypt a message" &= name "d"
                      ]
      , msg       = def &= help "The message to encrypt / decrypt" &= name "m"
      }
    &= help "Use Vigenere's encryption / decryption algorithm"

cipher =
  modes [caesar, vigenere]
    &= verbosity
    &= help "Encrypt and decrypt messages using various algorithms"
    &= summary "cipher-cli v0.0.0.1, (C) Vincibean"

codec :: Algo -> String
codec (Caesar Encrypt msg numKey) =
  maybe "Unencryptable messagge" extractDecryptable
    $   C.caesar numKey
    <$> encryptable msg
codec (Caesar Decrypt msg numKey) =
  maybe "Undecryptable messagge" extractEncryptable
    $   C.unCaesar numKey
    <$> decryptable msg
codec (Vigenere Encrypt msg stringKey) =
  maybe "Unencryptable messagge" extractDecryptable
    $   C.vigenere
    <$> key stringKey
    <*> encryptable msg
codec (Vigenere Decrypt msg stringKey) =
  maybe "Undecryptable messagge" extractEncryptable
    $   C.unVigenere
    <$> key stringKey
    <*> decryptable msg

main = print =<< codec <$> cmdArgs cipher
