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
  describe "caesar" $ do
    it "should ciper a specific string as intended" $
      caesar 3 "hello" `shouldBe` "khoor"
    it "should deciper a specific string as intended" $
      unCaesar 3 "khoor" `shouldBe` "hello"
    it "should cipher and decipher any word using any key" $
      property $ \(ASCIIString s) k -> (caesar k . unCaesar k) s === s
  describe "vigenere" $ do
    it "should ciper a specific string as intended" $
      vigenere "key" "hello" `shouldBe` "rijvs"
    it "should deciper a specific string as intended" $
      unVigenere "key" "rijvs" `shouldBe` "hello"
    it "should cipher and decipher any word using any key" $
      property $ \(ASCIIString s) (Key k) -> not (null k) ==> (vigenere k . unVigenere k) s === s
