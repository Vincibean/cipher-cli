module CipherSpec where

import Cipher
import Data.Char
import Test.Hspec
import Test.QuickCheck

newtype Key = Key String deriving (Eq, Show)

instance Arbitrary Key where
    arbitrary = Key <$> shuffle (['a'..'z'] ++ ['A'..'Z'])

spec :: Spec
spec = do
  describe "caesar" $
    it "should cipher and decipher any word using any key" $
      property $ \(ASCIIString s) k -> (caesar k . unCaesar k) s === s
  describe "vigenere" $
    it "should cipher and decipher any word using any key" $
      property $ \(ASCIIString s) (Key k) -> not (null k) ==> (vigenere k . unVigenere k) s === s
