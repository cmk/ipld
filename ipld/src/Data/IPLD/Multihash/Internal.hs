{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- |
-- Copyright   : 2018 Monadic GmbH
-- License     : BSD3
-- Maintainer  : kim@monadic.xyz, team@monadic.xyz
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Data.IPLD.Multihash.Internal
    ( HashAlgorithm (..)
    , Multihashable

    , getMultihashedDigest
    , getHashAlgorithm
    , getLength

    , toCode
    , fromCode
    , fromCryptonite
    , digestSize

    , _3
    )
where

import           Control.Monad (when)
import qualified Crypto.Hash as C
import qualified Data.Binary.Get as Binary
import           Data.Binary.VarInt (getVarInt)
import           Data.Proxy (Proxy(..))
import           Data.Word (Word16)

type Multihashable a = (C.HashAlgorithm a, FromCryptonite a)

getMultihashedDigest :: forall a. Multihashable a => Binary.Get (C.Digest a)
getMultihashedDigest = do
    algo <- getHashAlgorithm
    when (fromCryptonite (Proxy @a) /= algo) $ fail "Algorithm mismatch"
    len <- getLength
    dig <- Binary.getByteString len
    maybe (fail "Invalid Digest") pure $ C.digestFromByteString dig

getHashAlgorithm :: Binary.Get HashAlgorithm
getHashAlgorithm = do
    code <- Binary.getWord8 >>= getVarInt
    case fromCode code of
        Nothing -> fail ("Unknown HashAlgorithm " <> show code)
        Just ha -> pure ha

getLength :: Binary.Get Int
getLength = Binary.getWord8 >>= getVarInt

-- | 'Crypto.Hash.HashAlgorithm's for which we know a multihash code.
--
-- Note that this currently excludes variable output-length algorithms.
data HashAlgorithm =
      Blake2s_160
    | Blake2s_224
    | Blake2s_256
    | Blake2b_160
    | Blake2b_224
    | Blake2b_256
    | Blake2b_384
    | Blake2b_512
    | MD4
    | MD5
    | SHA1
    | SHA256
    | SHA512
    | Keccak_224
    | Keccak_256
    | Keccak_384
    | Keccak_512
    | SHA3_224
    | SHA3_256
    | SHA3_384
    | SHA3_512
    deriving (Eq, Enum, Bounded)

class FromCryptonite a where
    fromCryptonite :: proxy a -> HashAlgorithm

instance FromCryptonite C.Blake2s_160 where fromCryptonite _ = Blake2s_160
instance FromCryptonite C.Blake2s_224 where fromCryptonite _ = Blake2s_224
instance FromCryptonite C.Blake2s_256 where fromCryptonite _ = Blake2s_256
instance FromCryptonite C.Blake2b_160 where fromCryptonite _ = Blake2b_160
instance FromCryptonite C.Blake2b_224 where fromCryptonite _ = Blake2b_224
instance FromCryptonite C.Blake2b_256 where fromCryptonite _ = Blake2b_256
instance FromCryptonite C.Blake2b_384 where fromCryptonite _ = Blake2b_384
instance FromCryptonite C.Blake2b_512 where fromCryptonite _ = Blake2b_512
instance FromCryptonite C.MD4         where fromCryptonite _ = MD4
instance FromCryptonite C.MD5         where fromCryptonite _ = MD5
instance FromCryptonite C.SHA1        where fromCryptonite _ = SHA1
instance FromCryptonite C.SHA256      where fromCryptonite _ = SHA256
instance FromCryptonite C.SHA512      where fromCryptonite _ = SHA512
instance FromCryptonite C.Keccak_224  where fromCryptonite _ = Keccak_224
instance FromCryptonite C.Keccak_256  where fromCryptonite _ = Keccak_256
instance FromCryptonite C.Keccak_384  where fromCryptonite _ = Keccak_384
instance FromCryptonite C.Keccak_512  where fromCryptonite _ = Keccak_512
instance FromCryptonite C.SHA3_224    where fromCryptonite _ = SHA3_224
instance FromCryptonite C.SHA3_256    where fromCryptonite _ = SHA3_256
instance FromCryptonite C.SHA3_384    where fromCryptonite _ = SHA3_384
instance FromCryptonite C.SHA3_512    where fromCryptonite _ = SHA3_512

toCode :: HashAlgorithm -> Word16
toCode Blake2s_160 = 0xb254
toCode Blake2s_224 = 0xb25c
toCode Blake2s_256 = 0xb260
toCode Blake2b_160 = 0xb214
toCode Blake2b_224 = 0xb21c
toCode Blake2b_256 = 0xb220
toCode Blake2b_384 = 0xb230
toCode Blake2b_512 = 0xb240
toCode MD4         = 0xd4
toCode MD5         = 0xd5
toCode SHA1        = 0x11
toCode SHA256      = 0x12
toCode SHA512      = 0x13
toCode Keccak_224  = 0x1A
toCode Keccak_256  = 0x1B
toCode Keccak_384  = 0x1C
toCode Keccak_512  = 0x1D
toCode SHA3_224    = 0x17
toCode SHA3_256    = 0x16
toCode SHA3_384    = 0x15
toCode SHA3_512    = 0x14

fromCode :: Word16 -> Maybe HashAlgorithm
fromCode 0xb254 = pure Blake2s_160
fromCode 0xb25c = pure Blake2s_224
fromCode 0xb260 = pure Blake2s_256
fromCode 0xb214 = pure Blake2b_160
fromCode 0xb21c = pure Blake2b_224
fromCode 0xb220 = pure Blake2b_256
fromCode 0xb230 = pure Blake2b_384
fromCode 0xb240 = pure Blake2b_512
fromCode 0xd4   = pure MD4
fromCode 0xd5   = pure MD5
fromCode 0x11   = pure SHA1
fromCode 0x12   = pure SHA256
fromCode 0x13   = pure SHA512
fromCode 0x1A   = pure Keccak_224
fromCode 0x1B   = pure Keccak_256
fromCode 0x1C   = pure Keccak_384
fromCode 0x1D   = pure Keccak_512
fromCode 0x17   = pure SHA3_224
fromCode 0x16   = pure SHA3_256
fromCode 0x15   = pure SHA3_384
fromCode 0x14   = pure SHA3_512
fromCode _      = Nothing

digestSize :: HashAlgorithm -> Int
digestSize Blake2s_160 = C.hashDigestSize C.Blake2s_160
digestSize Blake2s_224 = C.hashDigestSize C.Blake2s_224
digestSize Blake2s_256 = C.hashDigestSize C.Blake2s_256
digestSize Blake2b_160 = C.hashDigestSize C.Blake2b_160
digestSize Blake2b_224 = C.hashDigestSize C.Blake2b_224
digestSize Blake2b_256 = C.hashDigestSize C.Blake2b_256
digestSize Blake2b_384 = C.hashDigestSize C.Blake2b_384
digestSize Blake2b_512 = C.hashDigestSize C.Blake2b_512
digestSize MD4         = C.hashDigestSize C.MD4
digestSize MD5         = C.hashDigestSize C.MD5
digestSize SHA1        = C.hashDigestSize C.SHA1
digestSize SHA256      = C.hashDigestSize C.SHA256
digestSize SHA512      = C.hashDigestSize C.SHA512
digestSize Keccak_224  = C.hashDigestSize C.Keccak_224
digestSize Keccak_256  = C.hashDigestSize C.Keccak_256
digestSize Keccak_384  = C.hashDigestSize C.Keccak_384
digestSize Keccak_512  = C.hashDigestSize C.Keccak_512
digestSize SHA3_224    = C.hashDigestSize C.SHA3_224
digestSize SHA3_256    = C.hashDigestSize C.SHA3_256
digestSize SHA3_384    = C.hashDigestSize C.SHA3_384
digestSize SHA3_512    = C.hashDigestSize C.SHA3_512

--------------------------------------------------------------------------------

_3 :: (a, b, c) -> c
_3 (_, _, x) = x
