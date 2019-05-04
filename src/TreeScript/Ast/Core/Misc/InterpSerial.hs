{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | One-way specialized @MessagePack@ serialization for @Core@ ASTs, for the interpreter. 'Data.Binary' serializes for the compiler.
-- Hacked from https://github.com/msgpack/msgpack-haskell/blob/master/msgpack/src/Data/MessagePack/Generic.hs
module TreeScript.Ast.Core.Misc.InterpSerial
  ( InterpSerial (..)
  , encodeInterp
  ) where

import TreeScript.Misc

import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Data.MessagePack
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics

newtype Tagged (s :: * -> *) b = Tagged{ unTagged :: b }

class InterpSerial a where
  toMsgp :: a -> Object
  default toMsgp :: (Generic a, InterpSerial' (Rep a)) => a -> Object
  toMsgp = toMsgp' . from
  skipMsgp :: Proxy a -> Bool
  default skipMsgp :: (Generic a, InterpSerial' (Rep a)) => Proxy a -> Bool
  skipMsgp Proxy = False

class InterpSerial' f where
  toMsgp' :: f a -> Object
  skipMsgp' :: Proxy f -> Bool
  isProd :: Proxy f -> Bool
  isProd Proxy = False

class InterpSerialProd f where
  toMsgpProd :: f a -> [Object]

class SumSize f where
  sumSize :: Tagged f Int

class (SumSize f) => InterpSerialSum f where
  toMsgpSum :: Int -> Int -> f a -> Object

instance InterpSerial' V1 where
  toMsgp' = undefined
  skipMsgp' Proxy = undefined

instance InterpSerial' U1 where
  toMsgp' U1 = undefined
  skipMsgp' Proxy = True

instance (InterpSerial' a, InterpSerialProd b) => InterpSerial' (a :*: b) where
  toMsgp' xs = ObjectArray $ toMsgpProd xs
  skipMsgp' Proxy = False
  isProd Proxy = True

instance (InterpSerialSum f, InterpSerialSum g) => InterpSerial' (f :+: g) where
  toMsgp' = toMsgpSum 0 size
    where size = unTagged (sumSize :: Tagged (f :+: g) Int)
  skipMsgp' Proxy = False

instance InterpSerial' a => InterpSerial' (M1 t c a) where
  toMsgp' (M1 x) = toMsgp' x
  skipMsgp' Proxy = skipMsgp' (Proxy :: Proxy a)

instance InterpSerial a => InterpSerial' (K1 i a) where
  toMsgp' (K1 x) = toMsgp x
  skipMsgp' Proxy = skipMsgp (Proxy :: Proxy a)

-- Product type packing.

instance (InterpSerial' a, InterpSerialProd b) => InterpSerialProd (a :*: b) where
  toMsgpProd (x :*: xs)
    | skipMsgp' (Proxy :: Proxy a) = toMsgpProd xs
    | isProd (Proxy :: Proxy a)
    = case toMsgp' x of
        ObjectArray ys -> ys ++ toMsgpProd xs
        _ -> error "unexpected: generic product didn't serialize to array"
    | otherwise = toMsgp' x : toMsgpProd xs

instance InterpSerial' a => InterpSerialProd (M1 t c a) where
  toMsgpProd (M1 x)
    | skipMsgp' (Proxy :: Proxy a) = []
    | otherwise = [toMsgp' x]

-- Sum type packing.

instance (InterpSerialSum a, InterpSerialSum b) => InterpSerialSum (a :+: b) where
  toMsgpSum code size (L1 x) = toMsgpSum code sizeL x
    where sizeL = size `shiftR` 1
  toMsgpSum code size (R1 x) = toMsgpSum (code + sizeL) sizeR x
    where sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance (InterpSerial' a) => InterpSerialSum (C1 c a) where
  toMsgpSum code _ x
    | skipMsgp' (Proxy :: Proxy a) = ObjectInt $ fromIntegral code
    | isProd (Proxy :: Proxy a) = ObjectArray [ObjectInt $ fromIntegral code, toMsgp' x]
    | otherwise = ObjectArray [ObjectInt $ fromIntegral code, ObjectArray [toMsgp' x]]

-- Sum size.

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
  sumSize
    = Tagged
    $ unTagged (sumSize :: Tagged a Int)
    + unTagged (sumSize :: Tagged b Int)

instance SumSize (C1 c a) where
  sumSize = Tagged 1

-- Builtins.

instance InterpSerial () where
  toMsgp = undefined
  skipMsgp Proxy = True

instance InterpSerial Int where
  toMsgp = toObject
  skipMsgp Proxy = False

instance InterpSerial Float where
  toMsgp = toObject
  skipMsgp Proxy = False

instance InterpSerial T.Text where
  toMsgp = toObject
  skipMsgp Proxy = False

instance InterpSerial B.ByteString where
  toMsgp = ObjectBin . B.toStrict
  skipMsgp Proxy = False

instance InterpSerial Range where
  toMsgp _ = undefined
  skipMsgp Proxy = True

instance (InterpSerial a) => InterpSerial [a] where
  toMsgp = ObjectArray . map toMsgp
  skipMsgp Proxy = skipMsgp (Proxy :: Proxy a)

instance (InterpSerial a, InterpSerial b) => (InterpSerial (a, b)) where
  toMsgp (x, y) = ObjectArray [toMsgp x, toMsgp y]
  skipMsgp Proxy = False

instance (InterpSerial k, InterpSerial v) => InterpSerial (M.Map k v) where
  toMsgp = toMsgp . M.toAscList
  skipMsgp Proxy = skipMsgp (Proxy :: Proxy v)

encodeInterp :: InterpSerial a => a -> B.ByteString
encodeInterp = pack . toMsgp