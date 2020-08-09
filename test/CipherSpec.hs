module CipherSpec where

import Cipher
import Data.Char
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Types

newtype Key = Key String deriving (Eq, Show)

instance Arbitrary Key where
  arbitrary = Key <$> shuffle (['a' .. 'z'] ++ ['A' .. 'Z'])

newtype ArbEncryptable = ArbEncryptable Encryptable deriving (Eq, Show)

instance Arbitrary ArbEncryptable where
  arbitrary = (ArbEncryptable . fromJust) . encryptable <$> shuffle (['a' .. 'z'] ++ ['A' .. 'Z'])

spec :: Spec
spec = do
  describe "caesar" $ do
    it "should ciper a specific string as intended" $ do
      let hello = fromJust $ encryptable "hello"
      let res = extractDecryptable . caesar 3 $ hello
      res `shouldBe` "khoor"
    it "should ciper a specific string as intended, even when the key is greater than the size of the alphabet" $ do
      let hello = fromJust $ encryptable "hello"
      let res = extractDecryptable . caesar 29 $ hello
      res `shouldBe` "khoor"
    it "should deciper a specific string as intended" $ do
      let khoor = fromJust $ decryptable "khoor"
      let res = extractEncryptable . unCaesar 3 $ khoor
      res `shouldBe` "hello"
    it "should cipher and decipher any word using any key" $
      property $ \(ArbEncryptable s) k -> (extractEncryptable . unCaesar k . caesar k) s === extractEncryptable s
  describe "vigenere" $ do
    it "should ciper a specific string as intended" $ do
      let hello = fromJust $ encryptable "hello"
      let res = extractDecryptable . vigenere "key" $ hello
      res `shouldBe` "rijvs"
    it "should deciper a specific string as intended" $ do
      let rijvs = fromJust $ decryptable "rijvs"
      let res = extractEncryptable . unVigenere "key" $ rijvs
      res `shouldBe` "hello"
    it "should cipher and decipher any word using any key" $
      property $ \(ArbEncryptable s) (Key k) -> not (null k) ==> (extractEncryptable . unVigenere k . vigenere k) s === extractEncryptable s
