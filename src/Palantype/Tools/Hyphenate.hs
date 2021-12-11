{-# LANGUAGE DerivingStrategies #-}
module Palantype.Tools.Hyphenate where

import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($) )
import           Data.List                      ( intersperse )
import           Data.Monoid                    ( Monoid(mconcat) )
import           Data.Text                      ( Text )
import           TextShow                       ( TextShow(showb)
                                                , fromText
                                                )
import Control.Category ((<<<))

newtype Hyphenated = Hyphenated { unHyphenated :: [Text] }
  deriving stock Eq

instance TextShow Hyphenated where
    showb (Hyphenated txts) = fromText (mconcat $ intersperse "|" txts)

toWord :: Hyphenated -> Text
toWord = mconcat <<< unHyphenated
