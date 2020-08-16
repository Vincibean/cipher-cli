module CipherSpec where

import           Cipher
import           Data.Char
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck
import           Types

newtype ArbKey = ArbKey Key deriving (Eq, Show)

instance Arbitrary ArbKey where
  arbitrary = (ArbKey . fromJust) . key <$> listOf1
    (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

newtype ArbEncryptable = ArbEncryptable Encryptable deriving (Eq, Show)

instance Arbitrary ArbEncryptable where
  arbitrary = (ArbEncryptable . fromJust) . encryptable <$> listOf1
    (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

spec :: Spec
spec = do
  describe "caesar" $ do
    it "should ciper a specific string as intended" $ do
      let hello = fromJust $ encryptable "hello"
      let res   = extractDecryptable . caesar 3 $ hello
      res `shouldBe` "khoor"
    it
        "should ciper a specific string as intended, even when the key is greater than the size of the alphabet"
      $ do
          let hello = fromJust $ encryptable "hello"
          let res   = extractDecryptable . caesar 29 $ hello
          res `shouldBe` "khoor"
    it
        "should ciper a specific string as intended, even when the key is negative"
      $ do
          let hello = fromJust $ encryptable "khoor"
          let res   = extractDecryptable . caesar (-3) $ hello
          res `shouldBe` "hello"
    it "should deciper a specific string as intended" $ do
      let khoor = fromJust $ decryptable "khoor"
      let res   = extractEncryptable . unCaesar 3 $ khoor
      res `shouldBe` "hello"
    it "should cipher and decipher any word using any key"
      $ property
      $ \(ArbEncryptable s) k -> (extractEncryptable . unCaesar k . caesar k) s
          === extractEncryptable s
  describe "vigenere" $ do
    it "should ciper a specific string as intended" $ do
      let hello = fromJust $ encryptable "hello"
      let k     = fromJust $ key "key"
      let res   = extractDecryptable . vigenere k $ hello
      res `shouldBe` "rijvs"
    it "should deciper a specific string as intended" $ do
      let rijvs = fromJust $ decryptable "rijvs"
      let k     = fromJust $ key "key"
      let res   = extractEncryptable . unVigenere k $ rijvs
      res `shouldBe` "hello"
    it "should cipher and decipher any word using any key"
      $ property
      $ \(ArbEncryptable s) (ArbKey k) ->
          (extractEncryptable . unVigenere k . vigenere k) s
            === extractEncryptable s
