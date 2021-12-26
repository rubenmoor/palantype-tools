{-# LANGUAGE RecordWildCards #-}

module Palantype.Tools.Collision where

import           Data.Bool                      ( Bool (..))
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($) )
import qualified Data.Map.Strict           as Map
import           Data.Int                       ( Int )
import           Data.List                      ( delete
                                                , length
                                                , (++), sortOn
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
import           Palantype.Common.RawSteno      ( RawSteno )
import Data.Foldable (Foldable(foldl'))
import Data.Functor ((<&>))
import Data.Tuple (snd)
import Data.Map.Strict (Map)

data DictState = DictState
    { dstMapWordStenos :: Map Text [RawSteno]
    , dstMapStenoWord :: Map RawSteno Text
    }

resolve :: Text -> [RawSteno] -> DictState -> (DictState, Bool)
resolve word raws dst@DictState{..} =
    let
        -- look up collisions ...
        mNAlts raw = Map.lookup raw dstMapStenoWord <&> \collision ->
            -- ... and for every collisions the number of alternatives
            -- of the existing word, i.e. the gravity of the collision
            -- where 1 implicates that `raw` is inviable (soft squeeze B)
            (length $ fromMaybe (error "resolve: impossible") $
                     Map.lookup collision dstMapWordStenos
            -- and while we're at it, save the colliding word, too
            , collision)

        -- sort to make sure that inviable raws get sorted out first
        rawsCollisions = sortOn snd $ raws <&> \raw -> (raw, mNAlts raw)

        (_, dst', isLost) =
            foldl' (accAllocate word) (length raws, dst, False) rawsCollisions

    in  (dst', isLost)

accAllocate
  :: Text
  -> (Int, DictState, Bool)
  -> (RawSteno, Maybe (Int, Text))
  -> (Int, DictState, Bool)

-- no collision
accAllocate word (nAlts, DictState{..}, _) (raw, Nothing) =
    ( nAlts
    , DictState
        { dstMapWordStenos = Map.insertWith (++) word [raw] dstMapWordStenos
        , dstMapStenoWord  = Map.insert raw word dstMapStenoWord}
    , False
    )

-- collision
accAllocate word
            (nAlts, dst@DictState{..}, _)
            (raw, Just (collisionNAlts, collisionWord)) =

    case (nAlts, collisionNAlts) of

        -- hard squeeze: existing word wins; new word is lost entirely
        (1, 1) -> (nAlts, dst, True)

        -- soft squeeze A: new word w/o alternatives; existing word loses one alternative
        (1, _) -> (nAlts, DictState
                  { dstMapWordStenos =
                        Map.insert word [raw] $
                        Map.adjust (delete raw) collisionWord dstMapWordStenos
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
                  { dstMapWordStenos = Map.insertWith (++) word [raw] $
                        Map.adjust (delete raw) collisionWord dstMapWordStenos
                  , dstMapStenoWord  =
                        Map.insert raw word dstMapStenoWord
                  }, False)

        -- avoid missing patterns warning
        _ -> error "resolve: impossible"
