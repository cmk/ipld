{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Data.IPLD.Multihash where


import           Hedgehog
import qualified Crypto.Hash as C
import qualified Data.IPLD.Multihash as Strict
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

props :: IO Bool
props = checkParallel $$(discover)

prop_strict_rt :: Property
prop_strict_rt = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Strict.multihash C.Blake2b_256 bs
        bytes   = Strict.encodedBytes hashed
        decoded = Strict.fromDigest @C.Blake2b_256 <$> Strict.decodeDigest bytes
     in
        assert $ decoded == Right hashed

prop_strict_rt_bytes :: Property
prop_strict_rt_bytes = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Strict.multihash C.Blake2b_256 bs
        bytes   = Strict.encodedBytes hashed
        decoded = Strict.decode bytes
     in
        assert $ decoded == Right hashed

prop_strict_algorithm_mismatch :: Property
prop_strict_algorithm_mismatch = property $ do
    bs <- forAll $ Gen.bytes (Range.singleton 128)
    let
        hashed  = Strict.multihash C.Blake2b_256 bs
        bytes   = Strict.encodedBytes hashed
        decoded = Strict.fromDigest @C.SHA256 <$> Strict.decodeDigest bytes
     in do
        annotate (render decoded)
        assert $ decoded == Left "Algorithm mismatch"

render :: Either String a -> String
render (Left  s) = "Left " <> s
render (Right _) = "Right <multihash>"
