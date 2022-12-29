{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Palantype.Tools.Collision where

import           Control.Category               ( (<<<) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($)
                                                , on, flip
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Int                       ( Int )
import           Data.List                      ( deleteBy
                                                , length
                                                , sortOn
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , fromMaybe
                                                )
import           Data.Ord                       ( Ord((<), (>)) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           GHC.Enum                       ( maxBound )
import           GHC.Err                        ( error )
import           GHC.Num                        ( (-) )
import           Palantype.Common               ( Greediness
                                                , Palantype(PatternGroup)
                                                , RawSteno
                                                )
import Data.Bool (Bool (True, False))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import TextShow (TextShow (showb))

default (Int)

data CollisionInfo = CollisionInfo
    { -- | word that looses a steno chord
      ciLosingWord     :: Text
      -- | word that competes for the same steno
    , ciWinningWord    :: Text
      -- | steno code that caused the conflict
    , ciRawSteno       :: RawSteno
      -- | whether the loosing word is lost entirely for lack of alternatives
    , ciIsLostEntirely :: Bool
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

data StenoCodeInfo key = StenoCodeInfo
    { -- | the index orders the codes by efficiency, 0 being most efficient
      sciIndex :: Int
      -- | the raw steno code
    , sciRawSteno :: RawSteno
      -- | the level: the highest pattern group that was applied to reach this
      --   code, at identical pattern groups,
      --   the levels are differentiated by the pattern
    , sciLevel :: (PatternGroup key, Greediness)
    }
    deriving stock (Generic)

deriving anyclass instance Palantype key => NFData (StenoCodeInfo key)

instance Palantype key => TextShow (StenoCodeInfo key) where
  showb StenoCodeInfo{..} =
       showb sciIndex    <> " "
    <> showb sciRawSteno <> " "
    <> showb sciLevel    <> " "

toStenoCodeInfo
  :: (Int, (RawSteno, (PatternGroup key, Greediness)))
  -> StenoCodeInfo key
toStenoCodeInfo (i, (raw, (pg, g))) = StenoCodeInfo i raw (pg, g)

data DictState key = DictState
    {
      -- | store index with the steno code, to restore original order after
      --   collision resolution
      dstMapWordStenos
          :: Map Text [StenoCodeInfo key]
    , dstMapStenoWord :: Map RawSteno Text
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

resolve
    :: forall key
     . Text                -- ^ input word
    -> [StenoCodeInfo key] -- ^ candidates
    -> DictState key       -- ^ old dict state
    -> (DictState key, [CollisionInfo])

resolve word raws dst@DictState {..} =
    let
        -- look up collisions ...
        mNAlts :: RawSteno -> (Int, Maybe Text)
        mNAlts raw = case Map.lookup raw dstMapStenoWord of
            Just collision ->
              -- ... and for every collisions the number of alternatives
              -- of the existing word, i.e. the gravity of the collision
              -- where 1 implicates that `raw` is inviable (soft squeeze B)
                ( length $ fromMaybe (error "resolve: impossible") $ Map.lookup
                    collision
                    dstMapWordStenos
              -- and while we're at it, save the colliding word, too
                , Just collision
                )
            Nothing -> (maxBound, Nothing)

        -- sort to make sure that inviable raws get sorted out first
        rawsCollisions :: [StenoCodeInfo key] -> [(StenoCodeInfo key, (Int, Maybe Text))]
        rawsCollisions infos =
            sortOn (fst <<< snd) $ infos <&> \info@StenoCodeInfo{..} ->
                (info, mNAlts sciRawSteno)

        (_, dst', cis) =
            foldl' (accAllocate word)
                   (length raws, dst, [])
                   (rawsCollisions raws)
    in
        (dst', cis)

accAllocate
    :: forall key
     . Text
    -> (Int, DictState key, [CollisionInfo])
    -> ( StenoCodeInfo key
       , (Int, Maybe Text)
       )
    -> (Int, DictState key, [CollisionInfo])

-- no collision
accAllocate word (nAlts, DictState {..}, ci) (info, (_, Nothing)) =
    ( nAlts
    , DictState
        { dstMapWordStenos = Map.insertWith (flip (<>)) word [info] dstMapWordStenos
        , dstMapStenoWord  = Map.insert (sciRawSteno info) word dstMapStenoWord
        }
    , ci
    )


-- collision
accAllocate word (nAlts, dst@DictState {..}, ci) (info, (collisionNAlts, Just collisionWord))
    = case (nAlts, collisionNAlts) of

            -- hard squeeze: existing word wins; new word is lost entirely
            (1, 1) -> ( nAlts
                      , dst
                      , CollisionInfo word collisionWord raw True : ci
                      )

            -- soft squeeze A: new word w/o alternatives; existing word loses one alternative
            (1, _) ->
                ( nAlts
                , DictState
                    { dstMapWordStenos = Map.insertWith (flip (<>)) word [info]
                        $ Map.adjust deleteIRaw collisionWord dstMapWordStenos
                    , dstMapStenoWord  = Map.insert raw word dstMapStenoWord
                    }
                , CollisionInfo collisionWord word raw False : ci
                )

            -- soft squeeze B: existing word w/o alternatives; new word loses one alternative
            (_, 1) ->
              ( nAlts - 1
              , dst
              , CollisionInfo word collisionWord raw False : ci
              )

            -- equal number of alternatives: new word loses
            _ | nAlts == collisionNAlts ->
                ( nAlts - 1
                , dst
                , CollisionInfo word collisionWord raw False : ci
                )

            -- new word has more alternatives: new word loses
            _ | nAlts > collisionNAlts ->
                ( nAlts - 1
                , dst
                , CollisionInfo word collisionWord raw False : ci
                )

            -- new word has less alternatives: new word wins, existing word loses one alternative
            _ | nAlts < collisionNAlts ->
                ( nAlts
                , DictState
                    { dstMapWordStenos =
                        Map.insertWith (flip (<>)) word [info]
                            $ Map.adjust deleteIRaw collisionWord dstMapWordStenos
                    , dstMapStenoWord  = Map.insert raw word dstMapStenoWord
                    }
                , CollisionInfo collisionWord word raw False : ci
                )

            -- avoid missing patterns warning
            _ -> error "resolve: impossible"

    where
      raw = sciRawSteno info
      deleteIRaw = deleteBy ((==) `on` sciRawSteno) info
