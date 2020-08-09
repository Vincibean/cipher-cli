module Cipher (caesar, unCaesar, vigenere, unVigenere) where

import Data.Char
import Data.List (elemIndex)

minOrdUpper :: Int
minOrdUpper = ord 'A'

minOrdLower :: Int
minOrdLower = ord 'a'

lowers = ['a' .. 'z']

uppers = ['A' .. 'Z']

caesar :: Int -> String -> String
caesar n = map $ shift n

unCaesar :: Int -> String -> String
unCaesar n = map $ shift (- n)

shift :: Int -> Char -> Char
shift n c
  | isLower c && n >= 0 = shift' n c lowers
  | isLower c && n < 0 = shift' (- n) c (reverse lowers)
  | isUpper c && n >= 0 = shift' n c uppers
  | isUpper c && n < 0 = shift' (- n) c (reverse uppers)
  | otherwise = c

shift' :: Int -> Char -> String -> Char
shift' n c chars = case elemIndex c chars of
  Nothing -> c
  Just i -> cycle chars !! shifted
    where
      shifted = i + n

type Key = String

type Message = String

type EncodedMessage = String

vigenere :: Key -> Message -> EncodedMessage
vigenere = vigenereCodec posShift

unVigenere :: Key -> EncodedMessage -> Message
unVigenere = vigenereCodec negShift

vigenereCodec :: (Char -> Int) -> Key -> String -> String
vigenereCodec f k m = z
  where
    x = zipLetters (cycle k) m
    y = map (\(k, m) -> (f k, m)) x
    z = map (uncurry shift) y

zipLetters :: String -> String -> [(Char, Char)]
zipLetters "" _ = []
zipLetters _ "" = []
zipLetters s1@(h1 : t1) s2@(h2 : t2)
  | not $ isLetter h1 = (h1, h1) : zipLetters t1 s2
  | not $ isLetter h2 = (h2, h2) : zipLetters s1 t2
  | otherwise = (h1, h2) : zipLetters t1 t2

posShift :: Char -> Int
posShift c
  | isLower c = ord c - minOrdLower
  | isUpper c = ord c - minOrdUpper
  | otherwise = 0

negShift :: Char -> Int
negShift = negate . posShift
