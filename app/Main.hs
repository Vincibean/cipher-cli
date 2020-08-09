{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Lib
import           System.Console.CmdArgs

data Op = Encrypt | Decrypt deriving (Show, Data, Typeable)

data Algo = Caesar {op :: Op, msg :: String, numKey :: Int} | Vigenere {op :: Op, msg :: String, stringKey :: String} deriving (Show, Data, Typeable)

caesar =
  Caesar
      { numKey = def
      , op     = enum
        [Encrypt &= help "Encrypt message", Decrypt &= help "Decrypt message"]
      , msg    = def &= help "Message"
      }
    &= help "Use Caesar's algorithm"

vigenere =
  Vigenere
      { stringKey = def
      , op        = enum
                      [ Encrypt &= help "Encrypt message" &= name "e"
                      , Decrypt &= help "Decrypt message" &= name "d"
                      ]
      , msg       = def &= help "Message" &= name "m"
      }
    &= help "Use Vigenere's algorithm"

main = print =<< cmdArgs (modes [caesar, vigenere])
