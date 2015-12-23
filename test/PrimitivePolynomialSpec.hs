{-# LANGUAGE ScopedTypeVariables #-}
module PrimitivePolynomialSpec (spec) where

import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.List as List
import           Data.Proxy (Proxy(Proxy))
import           Data.Word (Word8, Word16, Word32, Word64)
import           Test.Hspec

import qualified PrimitivePolynomial


spec :: Spec
spec = do
  describe "Int8" $
    it "sequence is faithful" $
      shouldBeFaithful (Proxy :: Proxy Int8)

  describe "Int16" $
    it "sequence is faithful" $
      shouldBeFaithful (Proxy :: Proxy Int16)

  describe "Int32" $
    it "sequence is faithful" $ do
      pendingWith "too slow"
      shouldBeFaithful (Proxy :: Proxy Int32)

  describe "Int64" $
    it "sequence is faithful" $ do
      pendingWith "too slow"
      shouldBeFaithful (Proxy :: Proxy Int64)

  describe "Word8" $
    it "sequence is faithful" $
      shouldBeFaithful (Proxy :: Proxy Word8)

  describe "Word16" $
    it "sequence is faithful" $
      shouldBeFaithful (Proxy :: Proxy Word16)

  describe "Word32" $
    it "sequence is faithful" $ do
      pendingWith "too slow"
      shouldBeFaithful (Proxy :: Proxy Word32)

  describe "Word64" $
    it "sequence is faithful" $ do
      pendingWith "too slow"
      shouldBeFaithful (Proxy :: Proxy Word64)

shouldBeFaithful
  :: forall p t. (PrimitivePolynomial.Gen t, Integral t, Bounded t, Ord t, Show t) => p t -> Expectation
shouldBeFaithful _ =
  List.sort (take (fromIntegral (maxBound :: t)) PrimitivePolynomial.sequence) `shouldBe` [1 .. maxBound :: t]
