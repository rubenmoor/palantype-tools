{-# LANGUAGE RecordWildCards #-}

module Palantype.Tools.Collision where

import           Data.Bool                      ( Bool (..))
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($) )
import           Data.HashMap.Internal.Strict   ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.List                      ( delete
                                                , length
                                                , (++)
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

data DictState = DictState
    { dstMapWordStenos :: HashMap Text [RawSteno]
    , dstMapStenoWord :: HashMap RawSteno Text
    }

resolve :: Text -> (Int, DictState, Bool) -> RawSteno -> (Int, DictState, Bool)
resolve word (nAlts, dst@DictState{..}, _) raw =
  case HashMap.lookup raw dstMapStenoWord of
    Nothing -> (nAlts, DictState
                 { dstMapWordStenos = HashMap.insertWith (++) word [raw] dstMapWordStenos
                 , dstMapStenoWord  = HashMap.insert raw word dstMapStenoWord
                 }, False)
    Just collisionWord ->
      let collisionNAlts = length $ fromMaybe (error "resolve: impossible") $ HashMap.lookup collisionWord dstMapWordStenos
      in  case (nAlts, collisionNAlts) of

            -- hard squeeze: existing word wins; new word is lost entirely
            (1, 1) -> (nAlts, dst, True)

            -- soft squeeze A: new word w/o alternatives; existing word loses one alternative
            (1, _) -> (nAlts, DictState
                      { dstMapWordStenos = HashMap.insert word [raw] $ HashMap.adjust (delete raw) collisionWord dstMapWordStenos
                      , dstMapStenoWord  = HashMap.insert raw word dstMapStenoWord
                      }, False)

            -- soft squeeze B: existing word w/o alternatives; new word loses one alternative
            (_, 1) -> (nAlts - 1, dst, False)

            -- equal number of alternatives: new word loses
            _ | nAlts == collisionNAlts -> (nAlts - 1, dst, False)

            -- new word has more alternatives: new word loses
            _ | nAlts > collisionNAlts -> (nAlts - 1, dst, False)

            -- new word has less alternatives: new word wins, existing word loses one alternative
            _ | nAlts < collisionNAlts -> (nAlts, DictState
                      { dstMapWordStenos = HashMap.insertWith (++) word [raw] $ HashMap.adjust (delete raw) collisionWord dstMapWordStenos
                      , dstMapStenoWord  = HashMap.insert raw word dstMapStenoWord
                      }, False)
