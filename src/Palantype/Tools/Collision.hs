{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Palantype.Tools.Collision where

import           Data.Bool                      ( Bool (..))
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($), on )
import qualified Data.Map.Strict           as Map
import           Data.Int                       ( Int )
import           Data.List                      ( length
                                                , (++), sortOn, zip, deleteBy
                                                )
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , fromMaybe


                                                )
import           Data.Ord                       ( Ord((>), (<))

                                                )
import           Data.Text                      ( Text



                                                )
import           GHC.Err                        ( error )
import           GHC.Num ((-))
import Data.Foldable (Foldable(foldl'))
import Data.Functor ((<&>))
import Data.Tuple (snd, fst)
import Data.Map.Strict (Map)
import Control.Category ((<<<))
import GHC.Enum (maxBound)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Strict.Tuple (Pair ((:!:)))
import Data.Strict (Strict(toLazy))
import Data.Bifunctor (Bifunctor(second))
import Palantype.Common
    ( RawSteno, Palantype(PatternGroup), Greediness )

default (Int)

data DictState key = DictState
    {
      -- | store index with the steno code, to restore original order after
      --   collision resolution
      dstMapWordStenos :: Map Text [(Int, (RawSteno, (PatternGroup key, Greediness)))]
    , dstMapStenoWord :: Map RawSteno Text
    }

resolve
  :: forall key
  .  Text
  -> Vector (Pair RawSteno (Pair (PatternGroup key) Greediness))
  -> DictState key
  -> (DictState key, Bool)
resolve word raws dst@DictState{..} =
    let
        -- look up collisions ...
        mNAlts :: Pair RawSteno (Pair (PatternGroup key) Greediness) -> (Int, Maybe Text)
        mNAlts (raw :!: _) = case Map.lookup raw dstMapStenoWord of
          Just collision ->
            -- ... and for every collisions the number of alternatives
            -- of the existing word, i.e. the gravity of the collision
            -- where 1 implicates that `raw` is inviable (soft squeeze B)
            (length $ fromMaybe (error "resolve: impossible") $
                     Map.lookup collision dstMapWordStenos
            -- and while we're at it, save the colliding word, too
            , Just collision)
          Nothing -> (maxBound, Nothing)

        -- sort to make sure that inviable raws get sorted out first
        rawsCollisions =
            sortOn (fst <<< snd)
          $ zip [0..] (Vector.toList raws) <&> \(i, raw) -> ((i, second toLazy $ toLazy raw), mNAlts raw)

        (_, dst', isLost) =
            foldl' (accAllocate word) (Vector.length raws, dst, False) rawsCollisions

    in  (dst', isLost)

accAllocate
  :: forall key
  . Text
  -> (Int, DictState key, Bool)
  -> ((Int, (RawSteno, (PatternGroup key, Greediness))), (Int, Maybe Text))
  -> (Int, DictState key, Bool)

-- no collision
accAllocate word (nAlts, DictState{..}, _) (iRaw, (_, Nothing)) =
    ( nAlts
    , DictState
        { dstMapWordStenos = Map.insertWith (++) word [iRaw] dstMapWordStenos
        , dstMapStenoWord  = Map.insert (fst $ snd iRaw) word dstMapStenoWord}
    , False
    )

-- collision
accAllocate word
            (nAlts, dst@DictState{..}, _)
            (iRaw@(_, (raw, _)), (collisionNAlts, Just collisionWord)) =

    case (nAlts, collisionNAlts) of

        -- hard squeeze: existing word wins; new word is lost entirely
        (1, 1) -> (nAlts, dst, True)

        -- soft squeeze A: new word w/o alternatives; existing word loses one alternative
        (1, _) -> (nAlts, DictState
                  { dstMapWordStenos =
                        Map.insert word [iRaw] $
                        Map.adjust deleteIRaw collisionWord dstMapWordStenos
                  , dstMapStenoWord  =
                        Map.insert raw word dstMapStenoWord
                  }, False)

        -- soft squeeze B: existing word w/o alternatives; new word loses one alternative
        (_, 1) -> (nAlts - 1, dst, False)

        -- equal number of alternatives: new word loses
        _ | nAlts == collisionNAlts -> (nAlts - 1, dst, False)

        -- new word has more alternatives: new word loses
        _ | nAlts > collisionNAlts -> (nAlts - 1, dst, False)

        -- new word has less alternatives: new word wins, existing word loses one alternative
        _ | nAlts < collisionNAlts -> (nAlts, DictState
                  { dstMapWordStenos = Map.insertWith (++) word [iRaw] $
                        Map.adjust deleteIRaw collisionWord dstMapWordStenos
                  , dstMapStenoWord  =
                        Map.insert raw word dstMapStenoWord
                  }, False)

        -- avoid missing patterns warning
        _ -> error "resolve: impossible"
  where
    deleteIRaw = deleteBy ((==) `on` (fst <<< snd)) iRaw
