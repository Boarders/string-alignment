{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Alignment.NeedlemanWunsch where

import Data.Massiv.Array
import qualified Data.ByteString as BS
import Prelude hiding (read)
import Alignment.Types
import qualified Data.Matrix.Unboxed as U
import Data.Word (Word8)
import Data.List.Extra (minimumOn)


needlemanWunschMatrix ::
  forall m r .
  ( PrimMonad m
  , Mutable r Ix2 (Word, Direction)
  , MonadUnliftIO m
  , MonadThrow m
  ) => Sz2               -- ^ array size
    -> (Distance Word8)  -- ^ character metric
    -> Word              -- ^ gap cost
    -> BS.ByteString     -- ^ left character
    -> BS.ByteString     -- ^ right character
    -> m (Array r Ix2 (Word, Direction))  
needlemanWunschMatrix sz@(Sz2 m n) =
  ukkonenMatrix (min m n) sz

ukkonenMatrix ::
  forall m r .
  ( PrimMonad m
  , Mutable r Ix2 (Word, Direction)
  , MonadUnliftIO m
  , MonadThrow m
  ) => Int               -- ^ band distance
    -> Sz2               -- ^ array size
    -> (Distance Word8)  -- ^ character metric
    -> Word              -- ^ gap cost
    -> BS.ByteString     -- ^ left character
    -> BS.ByteString     -- ^ right character
    -> m (Array r Ix2 (Word, Direction))
ukkonenMatrix band sz@(Sz2 m n) dist gapCost leftChar topChar =
  createArray_ Par sz $ \scheduler marr -> do
    forM_ (0 ..: n - 1) $ \j -> writeM marr (0 :. j) (((fromIntegral j) * gapCost, LeftArrow))
    -- ^ fill the top edge
    forM_ (0 ..: m - 1) $ \i -> writeM marr (i :. 0) (((fromIntegral i) * gapCost, UpArrow))
    -- ^ fill the left edge
    forM_ (1 ..: m - 1) $
      \diagColStart ->
        do
          let leftDiagIter
                = rangeStepSize Seq diagColStart (-1) (Sz (min diagColStart band))
          -- ^ an iterator for each northeast diagonal starting from a left column
          iforSchedulerM_ scheduler leftDiagIter $ \r c -> writeF marr (r + 1) c
          -- ^ fill the entries in the 'left' diagonals in parallel
    forM_ (rangeStep' Seq (n - 1) (- 1) 1) $ --(1 ..: n - 1) $
      \diagRowLen ->
        do
          let bottomDiagIter = rangeStepSize Seq (m - 1) (-1) (Sz (min diagRowLen band))
          -- ^ an iterator for each northeast diagonal starting from the bottom row
          iforSchedulerM_ scheduler bottomDiagIter $ \r c -> writeF marr (r + 1) c
            -- ^ fill the entries in the 'bottom' diagonals in parallel
  where
    writeF
      :: MArray (PrimState m) r Ix2 (Word, Direction)
      -> Int
      -> Int
      -> m ()
    writeF marr i j = do
      leftCost    <- fst <$> readM marr (i     :. j - 1)
      topLeftCost <- fst <$> readM marr (i - 1 :. j - 1)
      topCost     <- fst <$> readM marr (i - 1 :. j    )
      let topElement  = topChar  `BS.index` j
      let leftElement = leftChar `BS.index` i
      let alignCost   = dist topElement leftElement
      let leftTotalCost = leftCost + gapCost
      let topTotalCost  = topCost  + gapCost
      let diagTotalCost = topLeftCost + alignCost
      let
        newEntry =
          minimumOn fst
            [ (leftTotalCost, LeftArrow)
            , (topTotalCost , UpArrow  )
            , (diagTotalCost, DiagArrow)
            ]
      writeM marr (i :. j) newEntry


type Distance a = a -> a -> Word
type TransitionCost = U.Matrix (Word, Word)
  
