{-# LANGUAGE TemplateHaskell #-}

module Test.Data.IPLD.Graph.Tree where

import Crypto.Hash
import Crypto.Hash.IO (HashAlgorithm(..))
import Data.IPLD.Graph.Types
import Data.Eq.Deriving
import GHC.Generics
import Hedgehog
import Text.Show.Deriving
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as B
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Yaya.Unsafe.Fold as Unsafe


type FileBody = String
type PartialFilePath = String

data MockDirectoryTree x
  = Dir PartialFilePath [x]
  | File PartialFilePath FileBody
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

instance AE.ToJSON1 MockDirectoryTree
instance AE.FromJSON1 MockDirectoryTree
$(deriveShow1 ''MockDirectoryTree)
$(deriveEq1   ''MockDirectoryTree)

hashMockDirTree :: HashAlgorithm h => MockDirectoryTree (Digest h) -> Digest h
hashMockDirTree (File path body) = hash $ B.pack $ path <> body
hashMockDirTree (Dir path ds) = hashFinalize $ hashUpdate (hashUpdates hashInit ds) (B.pack path)

genDirAlg :: MonadGen m => Int -> m (MockDirectoryTree Int)
genDirAlg n = if n <= 0 then genFile else Gen.choice [genFile, genDir, genDir]
  where
    genDir = do
      name <- Gen.string  (Range.singleton 10) Gen.alphaNum
      subdirs <- Gen.list (Range.constant 1 3) (pure $ n - 1)
      pure $ Dir name subdirs

    genFile = do
      name <- Gen.string (Range.singleton 10) Gen.alphaNum
      body <- Gen.string (Range.singleton 20) Gen.alphaNum
      pure $ File name body

gen_dir_tree :: (MonadGen m, Steppable t MockDirectoryTree) => Int -> m t
gen_dir_tree = Unsafe.anaM genDirAlg
