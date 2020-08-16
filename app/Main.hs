{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Lib
import           System.Console.CmdArgs

data Op = Encrypt | Decrypt deriving (Show, Data, Typeable)

data Algo = Caesar {op :: Op, msg :: String, numKey :: Int} | Vigenere {op :: Op, msg :: String, stringKey :: String} deriving (Show, Data, Typeable)

caesar =
  Caesar
      { numKey = def &= help "The integer to use as key"
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
      { stringKey = def &= help "The string to use as key"
      , op        = enum
                      [ Encrypt &= help "Encrypt a message" &= name "e"
                      , Decrypt &= help "Decrypt a message" &= name "d"
                      ]
      , msg       = def &= help "The message to encrypt / decrypt" &= name "m"
      }
    &= help "Use Vigenere's encryption / decryption algorithm"

main = print =<< cmdArgs (modes [caesar, vigenere])
