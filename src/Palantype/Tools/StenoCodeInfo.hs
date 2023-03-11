{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Palantype.Tools.StenoCodeInfo where

import qualified Palantype.Common.Stage as Stage
import Palantype.Common
    ( Palantype(PatternGroup, patCapitalize, patAcronym, patSimpleMulti),
      Greediness,
      RawSteno,
      Stage (..),
      StageSpecialGeneric (..),
      StageIndex,
      StageSpecialGeneric(StageGeneric), patZero )
import Data.Int (Int)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import TextShow (TextShow (showb))
import Data.Semigroup (Semigroup((<>)))
import Data.Maybe ( Maybe(..) )
import Data.Functor ((<$>))
import Data.Eq (Eq((==)))
import Palantype.Common.Stage (stageIndexPatCapitalize, stageIndexPatAcronym, stageIndexPatZero, stageIndexPatSimpleMulti)
import Data.Bool (otherwise)

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

toStenoCodeInfoMaybe
  :: Palantype key
  => (Int, (RawSteno, StageIndex))
  -> Maybe (StenoCodeInfo key)
toStenoCodeInfoMaybe (i, (raw, si)) = StenoCodeInfo i raw <$> mPgg
  where
    mPgg = if
      | si == stageIndexPatZero        -> Just (patZero       , 0)
      | si == stageIndexPatSimpleMulti -> Just (patSimpleMulti, 0)
      | si == stageIndexPatCapitalize  -> Just (patCapitalize , 0)
      | si == stageIndexPatAcronym     -> Just (patAcronym    , 0)
      | otherwise -> do
          Stage sg _ <- Stage.fromIndex si
          case sg of
            StageGeneric pg g -> Just (pg, g)
            StageSpecial _    -> Nothing
