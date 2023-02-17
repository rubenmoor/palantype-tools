{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Palantype.Tools.StenoCodeInfo where
import Palantype.Common
    ( findStage,
      Palantype(PatternGroup),
      Greediness,
      RawSteno,
      StageSpecialGeneric(StageGeneric), mapStages )
import Palantype.Common.TH (failure)
import Data.Eq (Eq)
import Data.Ord (Ord (compare))
import Data.Int (Int)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import TextShow (TextShow (showb))
import Data.Semigroup (Semigroup((<>)))
import Data.Function (($))
import Data.Maybe (fromMaybe)
import GHC.Show (Show(show))

data StateStage key = StateStage
    { stPatternGroup :: PatternGroup key
    , stGreediness :: Greediness
    }

deriving stock instance Palantype key => Eq (StateStage key)

instance Palantype key => Ord (StateStage key) where
  compare (StateStage pg1 g1) (StateStage pg2 g2) =
      compare (toIndex pg1 g1) (toIndex pg2 g2)
    where
      toIndex pg g =
        fromMaybe ($failure $ "generic stage not found: " <> show pg <> show g) $
          findStage mapStages $ StageGeneric pg g

instance Palantype key => TextShow (StateStage key) where
  showb StateStage{..} = showb stPatternGroup <> showb stGreediness

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
  :: (Int, (RawSteno, StateStage key))
  -> StenoCodeInfo key
toStenoCodeInfo (i, (raw, StateStage pg g)) = StenoCodeInfo i raw (pg, g)
