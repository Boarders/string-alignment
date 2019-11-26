{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Alignment.Types where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U
import Data.Word (Word8)
import Control.Monad

data Direction = DiagArrow | LeftArrow | UpArrow
  deriving stock (Eq, Ord)


instance Show Direction where
    show DiagArrow = "↖"
    show LeftArrow = "←"
    show UpArrow   = "↑"


fromDirection :: Direction -> Word8
{-# INLINE fromDirection #-}
fromDirection DiagArrow = 0
fromDirection LeftArrow = 1
fromDirection UpArrow   = 2

toDirection :: Word8 -> Direction
{-# INLINE toDirection #-}
toDirection 0 = DiagArrow
toDirection 1 = LeftArrow
toDirection _ = UpArrow

data instance U.MVector s Direction = MV_Direction (P.MVector s Word8)
data instance U.Vector Direction    = V_Direction  (P.Vector    Word8)

instance U.Unbox Direction

instance M.MVector U.MVector Direction where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Direction v) = M.basicLength v
  basicUnsafeSlice i n (MV_Direction v) = MV_Direction $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Direction v1) (MV_Direction v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Direction `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Direction v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Direction `liftM` M.basicUnsafeReplicate n (fromDirection x)
  basicUnsafeRead (MV_Direction v) i = toDirection `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Direction v) i x = M.basicUnsafeWrite v i (fromDirection x)
  basicClear (MV_Direction v) = M.basicClear v
  basicSet (MV_Direction v) x = M.basicSet v (fromDirection x)
  basicUnsafeCopy (MV_Direction v1) (MV_Direction v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Direction v1) (MV_Direction v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Direction v) n = MV_Direction `liftM` M.basicUnsafeGrow v n


instance G.Vector U.Vector Direction where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Direction v) = V_Direction `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Direction v) = MV_Direction `liftM` G.basicUnsafeThaw v
  basicLength (V_Direction v) = G.basicLength v
  basicUnsafeSlice i n (V_Direction v) = V_Direction $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Direction v) i = toDirection `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Direction mv) (V_Direction v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
