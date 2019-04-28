{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Specialized @MessagePack@ serialization for @Core@ ASTs.
-- Hacked from https://github.com/msgpack/msgpack-haskell/blob/master/msgpack/src/Data/MessagePack/Generic.hs
module TreeScript.Ast.Core.Classes.Serial
  ( Serial (..)
  , serialize
  , deserialize
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.MessagePack
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics

newtype Tagged (s :: * -> *) b = Tagged{ unTagged :: b }

class Serial a where
  toMsgp :: a -> Object
  default toMsgp :: (Generic a, Serial' (Rep a)) => a -> Object
  toMsgp = toMsgp' . from
  fromMsgp :: Object -> Maybe a
  default fromMsgp :: (Generic a, Serial' (Rep a)) => Object -> Maybe a
  fromMsgp = fmap to . fromMsgp'
  skipMsgp :: Proxy a -> Bool
  skipMsgp _ = isJust (fromMsgp ObjectNil :: Maybe a)

class Serial' f where
  toMsgp' :: f a -> Object
  fromMsgp' :: Object -> Maybe (f a)
  skipMsgp' :: Proxy f -> Bool
  skipMsgp' _ = isJust (fromMsgp' ObjectNil :: Maybe (f a))
  toMsgpIfProd :: f a -> Maybe [Object]

class SerialProd f where
  toMsgpProd :: f a -> [Object]
  fromMsgpProd :: [Object] -> Maybe (f a)

class SumSize f where
  sumSize :: Tagged f Int

class (SumSize f) => SerialSum f where
  toMsgpSum :: Int -> Int -> f a -> Object
  fromMsgpSum :: Int -> Int -> Object -> Maybe (f a)

class (Serial' f) => SerialSumPart f where
  toMsgpSumPart :: f a -> Object
  fromMsgpSumPart :: Object -> Maybe (f a)

instance Serial' V1 where
  toMsgp' = undefined
  fromMsgp' _ = Nothing
  toMsgpIfProd _ = Nothing

instance Serial' U1 where
  toMsgp' U1 = ObjectNil
  fromMsgp' ObjectNil = Just U1
  fromMsgp' _ = Nothing
  toMsgpIfProd _ = Nothing

instance (Serial' a, SerialProd b) => Serial' (a :*: b) where
  toMsgp' xs = prodArray $ toMsgpProd xs
  fromMsgp' ObjectNil = (:*:) <$> fromMsgp' ObjectNil <*> fromMsgpProd []
  -- TODO: fromMsgp
  fromMsgp' (ObjectArray xs) = fromMsgpProd xs
  fromMsgp' _ = Nothing
  toMsgpIfProd = Just . toMsgpProd

instance (SerialSum f, SerialSum g) => Serial' (f :+: g) where
  toMsgp' = toMsgpSum 0 size
    where size = unTagged (sumSize :: Tagged (f :+: g) Int)
  fromMsgp' (ObjectInt code)
    | code' < size = fromMsgpSum code' size ObjectNil
    | otherwise = Nothing
    where code' = fromIntegral code
          size = unTagged (sumSize :: Tagged (f :+: g) Int)
  fromMsgp' (ObjectArray (ObjectInt code : xs))
    | code' < size = fromMsgpSum code' size $ ObjectArray xs
    | otherwise = Nothing
    where code' = fromIntegral code
          size = unTagged (sumSize :: Tagged (f :+: g) Int)
  fromMsgp' _ = Nothing
  toMsgpIfProd _ = Nothing

instance Serial' a => Serial' (M1 t c a) where
  toMsgp' (M1 x) = toMsgp' x
  fromMsgp' x = M1 <$> fromMsgp' x
  toMsgpIfProd _ = Nothing

instance Serial a => Serial' (K1 i a) where
  toMsgp' (K1 x) = toMsgp x
  fromMsgp' x = K1 <$> fromMsgp x
  toMsgpIfProd _ = Nothing

-- Product type packing.

instance (Serial' a, SerialProd b) => SerialProd (a :*: b) where
  toMsgpProd (x :*: xs)
    = case toMsgpIfProd x of
        Nothing -> toMsgp' x : toMsgpProd xs
        Just ys -> ys ++ toMsgpProd xs
  fromMsgpProd xs
    = fromMsgpProdNil <|> fromMsgProdReg xs
    where fromMsgpProdNil = (:*:) <$> fromMsgp' ObjectNil <*> fromMsgpProd xs
          fromMsgProdReg [] = Nothing
          fromMsgProdReg (y : ys) = (:*:) <$> fromMsgp' y <*> fromMsgpProd ys

instance Serial' a => SerialProd (M1 t c a) where
  toMsgpProd (M1 x) = [toMsgp' x]
  fromMsgpProd [] = M1 <$> fromMsgp' ObjectNil
  fromMsgpProd [x] = M1 <$> fromMsgp' x
  fromMsgpProd _ = Nothing

-- Sum type packing.

instance (SerialSum a, SerialSum b) => SerialSum (a :+: b) where
  toMsgpSum code size (L1 x) = toMsgpSum code sizeL x
    where sizeL = size `shiftR` 1
  toMsgpSum code size (R1 x) = toMsgpSum (code + sizeL) sizeR x
    where sizeL = size `shiftR` 1
          sizeR = size - sizeL
  fromMsgpSum code size x
    | code < sizeL = L1 <$> fromMsgpSum code sizeL x
    | otherwise = R1 <$> fromMsgpSum (code - sizeL) sizeR x
    where sizeL = size `shiftR` 1
          sizeR = size - sizeL

instance (SerialSumPart a) => SerialSum (C1 c a) where
  toMsgpSum code _ x
    | skipMsgp' (Proxy :: Proxy a) = ObjectInt $ fromIntegral code
    | otherwise = ObjectArray [ObjectInt $ fromIntegral code, toMsgpSumPart x]
  fromMsgpSum _ _ = fromMsgpSumPart

-- Sum size.

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
  sumSize
    = Tagged
    $ unTagged (sumSize :: Tagged a Int)
    + unTagged (sumSize :: Tagged b Int)

instance SumSize (C1 c a) where
  sumSize = Tagged 1

-- Sum part.

instance SerialSumPart U1 where
  toMsgpSumPart U1 = ObjectNil
  fromMsgpSumPart ObjectNil = Just U1
  fromMsgpSumPart _ = Nothing

instance (Serial' a) => SerialSumPart (M1 t c a) where
  toMsgpSumPart (M1 x) = ObjectArray [toMsgp' x]
  fromMsgpSumPart (ObjectArray [x]) = fromMsgp' x
  fromMsgpSumPart _ = Nothing

instance (Serial' a, SerialProd b) => SerialSumPart (a :*: b) where
  toMsgpSumPart xs = ObjectArray $ toMsgpProd xs
  fromMsgpSumPart (ObjectArray xs) = fromMsgpProd xs
  fromMsgpSumPart _ = Nothing

-- Builtins.

instance Serial () where
  toMsgp () = ObjectNil
  fromMsgp ObjectNil = Just ()
  fromMsgp _ = Nothing

instance Serial Int where
  toMsgp = toObject
  fromMsgp = fromObject

instance Serial Float where
  toMsgp = toObject
  fromMsgp = fromObject

instance Serial T.Text where
  toMsgp = toObject
  fromMsgp = fromObject

instance Serial B.ByteString where
  toMsgp = ObjectBin . B.toStrict
  fromMsgp = fromObject

instance (Serial a) => Serial [a] where
  toMsgp = ObjectArray . map toMsgp
  fromMsgp (ObjectArray xs) = traverse fromMsgp xs
  fromMsgp _ = Nothing

instance (Serial a, Serial b) => (Serial (a, b)) where
  toMsgp (x, y) = prodArray [toMsgp x, toMsgp y]
  fromMsgp ObjectNil = (,) <$> fromMsgp ObjectNil <*> fromMsgp ObjectNil
  fromMsgp (ObjectArray [x])
      = ((,) <$> fromMsgp x <*> fromMsgp ObjectNil)
    <|> ((,) <$> fromMsgp ObjectNil <*> fromMsgp x)
  fromMsgp (ObjectArray [x, y]) = (,) <$> fromMsgp x <*> fromMsgp y
  fromMsgp _ = Nothing

instance (Serial k, Serial v) => Serial (M.Map k v) where
  toMsgp = toMsgp . M.toAscList
  fromMsgp = fmap M.fromDistinctAscList . fromMsgp
  skipMsgp _ = False

prodArray :: [Object] -> Object
prodArray xs
  = case filter (/= ObjectNil) xs of
      [] -> ObjectNil
      [y] -> y
      ys -> ObjectArray ys


serialize :: Serial a => a -> B.ByteString
serialize = pack . toMsgp

deserialize :: Serial a => B.ByteString -> Maybe a
deserialize = fromMsgp <=< unpack
