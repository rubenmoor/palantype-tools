{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Category    (Category ((.)))
import           Data.Either         (Either (..))
import           Data.Eq             (Eq)
import           Data.Function       (($))
import           Data.Functor        ((<$>), Functor (fmap))
import           Data.Int            (Int)
import           Data.Ord            (Ord)
import           Data.Text           (Text, splitOn, concat)
import qualified Data.Text           as Text
import           Data.Text.IO        (interact)
import           System.IO           (IO)
import           Text.Parsec         (ParseError, Parsec, char, eof, letter,
                                      many1, parse, sepBy1, runParser)
import TextShow (TextShow(showt, showb), singleton)
import Data.Monoid (Monoid(mconcat))
import Data.List (intersperse)

main :: IO ()
main = interact parseSeries

data ParserState = ParserState
  { pstScore :: Int
  , pstFinger  :: Finger
  }

parseSeries :: Text -> Text
parseSeries str =
  let pstScore = 0
      pstKeys = FingerNone
  in  case runParser series ParserState{..} "pipe" str of
        Left err -> showt err
        Right s  -> showt s

-- chord :: Parsec Text ParserState Chord
-- chord =

-- a series of chords, to type a word of arbitrary length
newtype Series = Series { unSeries :: [Chord] }

instance TextShow Series where
  showb = mconcat . intersperse (singleton '/') . fmap showb . unSeries

newtype Chord = Chord { unChord :: [Key] }

instance TextShow Chord where
  showb = mconcat . fmap showb . unChord

  -- let syllables = splitOn "|" str
  -- in

-- capitalization:
--   no information available on the level of syllables
--   thus no concern here
-- input: he|r|um|trei|ber
-- TODO: input can have
--         '-', '/'
--         digits
--         ? other special characters?
--       otherwise:
--         a-zäüöß
--         international: é è, à, ê, ...
-- SOLUTION: parse word list for exceptions

-- the list of primitives is ordered
-- the primitive patterns will match in order
-- this has implications for patterns with same starting letter
-- e.g. "ge" must appear before "g"
-- otherwise the match will always be: "g" -> LeftGKGe, "e" -> LeftE
primitives :: [(Text, [Key])]
primitives =
  [ ("k"   , [LeftGKGe         ])
  , ("ge"  , [LeftGKGe         ])
  , ("g"   , [LeftGKGe         ])
  , ("be"  , [LeftBPBe         ])
  , ("b"   , [LeftBPBe         ])
  , ("p"   , [LeftBPBe         ])
  , ("h"   , [LeftH            ])
  , ("s"   , [LeftSChSchZTschTs])
  , ("ß"   , [LeftSChSchZTschTs])
  -- c,ch: cf. c below
  , ("sch" , [LeftSChSchZTschTs])
  , ("z"   , [LeftSChSchZTschTs])
  , ("tsch", [LeftSChSchZTschTs])
  , ("ts"  , [LeftSChSchZTschTs])
  , ("je"  , [LeftJ            ])
  , ("j"   , [LeftJ            ])
  , ("y"   , [LeftJ            ])
  , ("f"   , [LeftFVW          ])
  , ("ver" , [LeftFVW, LeftREr ])
  , ("v"   , [LeftFVW          ])
  , ("w"   , [LeftFVW          ])
  , ("de"  , [LeftDDeDiTTe     ])
  , ("di"  , [LeftDDeDiTTe     ]) -- risky?
  , ("d"   , [LeftDDeDiTTe     ])
  , ("te"  , [LeftDDeDiTTe     ])
  , ("t"   , [LeftDDeDiTTe     ])
  , ("m"   , [LeftM            ])
  , ("l"   , [LeftL            ])
  , ("n"   , [LeftN            ])
  , ("ent" , [LeftDDeDiTTe, LeftN    ]) -- ent -> TN
  , ("en"  , [LeftN            ])
  , ("r"   , [LeftREr          ])
  , ("er"  , [LeftREr          ])
  , ("e"   , [LeftE            ])
  , ("ä"   , [LeftE            ])
  , ("a"   , [LeftA            ])
  , ("o"   , [LeftO            ])
  , ("qu"  , [LeftGKGe, LeftFVW])       -- qu -> KW
  -- ambiguous
  -- c
  , ("ca"  , [LeftGKGe, LeftA])          -- phonetic K
  , ("cä"  , [LeftSChSchZTschTs, LeftA]) -- phonetic Z
  , ("ce"  , [LeftSChSchZTschTs, LeftE]) -- phonetic Tsch: Cellist, Cello
  -- ch
  -- TODO
  -- complicated by a lot of international words
  -- https://www.ids-mannheim.de/lexik/fremdwort/stichwortauswahl/lemmalisten/c/
  , ("chaise", [LeftCrossPrim, LeftSChSchZTschTs, LeftE, RightPointSnd, RightCross, RightSChSchZTsTschTzEs])
  , ("chau", [LeftSChSchZTschTs, LeftA, RightU])
  , ("champ",[LeftSChSchZTschTs, LeftE, RightNMEm])
  , ("cham", [LeftSChSchZTschTs, LeftA, RightPointSnd, RightNMEm])
  , ("chan", [LeftSChSchZTschTs, LeftA, RightPointSnd, RightNMEm])
  , ("charg",[LeftSChSchZTschTs, LeftA, RightRRe, RightSChSchZTsTschTzEs])
  , ("cha" , [LeftGKGe         , LeftA])
  -- TODO: exceptions. pronounciation of "char" depends on word
  -- archaisch, charmant
  -- TODO: exception: chassis: full word exception
  , ("cho" , [LeftGKGe, LeftO])
  , ("chö" , [LeftGKGe, LeftO, RightPointSnd])
  , ("chr" , [LeftGKGe, LeftREr])
  , ("ch"  , [LeftSChSchZTschTs])
  , ("ci"  , [LeftSChSchZTschTs, RightI])
  , ("c"   , [LeftGKGe])

  , ("ü"   , [RightUUmlautY    ])
  , ("y"   , [RightUUmlautY    ])
  , ("i"   , [RightI           ])
  , ("u"   , [RightU           ])
  , ("rr"  , [RightRRe         ])
  , ("r"   , [RightRRe         ])
  , ("ll"  , [RightL           ])
  , ("l"   , [RightL           ])
  , ("g"   , [RightGKCkCh      ])
  , ("k"   , [RightGKCkCh      ])
  , ("ck"  , [RightGKCkCh      ])
  -- for ch -> RightGKCkCh as in "Wachs", see "chs"
  , ("be"  , [RightBPBePe      ]) -- useful?
  , ("b"   , [RightBPBePe      ])
  , ("pe"  , [RightBPBePe      ]) -- useful?
  , ("pp"  , [RightBPBePe      ])
  , ("p"   , [RightBPBePe      ])
  , ("ff"  , [RightFVW         ])
  , ("f"   , [RightFVW         ])
  , ("v"   , [RightFVW         ])
  , ("w"   , [RightFVW         ])
  , ("ß"   , [RightSChSchZTsTschTzEs])
  , ("sch" , [RightSChSchZTsTschTzEs])
  , ("ss"  , [RightSChSchZTsTschTzEs])
  , ("s"   , [RightSChSchZTsTschTzEs])
  , ("z"   , [RightSChSchZTsTschTzEs])
  , ("chs" , [RightGKCkCh, RightSChSchZTsTschTzEs]) -- phonetically ks, like in "Wachs"
  , ("ch"  , [RightSChSchZTsTschTzEs])
  , ("tsch", [RightSChSchZTsTschTzEs])
  , ("ts"  , [RightSChSchZTsTschTzEs])
  , ("tz"  , [RightSChSchZTsTschTzEs])
  , ("es"  , [RightSChSchZTsTschTzEs])
  , ("ng"  , [RightGKCkCh , RightNMEm]) -- ng -> GN
  , ("nn"  , [RightNMEm        ])
  , ("n"   , [RightNMEm        ])
  , ("mm"  , [RightNMEm        ])
  , ("m"   , [RightNMEm        ])
  , ("em"  , [RightNMEm        ])
  , ("de"  , [RightDTTe        ])
  , ("d"   , [RightDTTe        ])
  , ("te"  , [RightDTTe        ])
  , ("tt"  , [RightDTTe        ])
  , ("t"   , [RightDTTe        ])
  , ("en"  , [RightEn          ])
  , ("el"  , [RightElEr        ])
  , ("lk"  , [RightGKCkCh , RightElEr]) -- lk -> KL
  , ("l"  , [RightElEr         ]) -- maybe for Austria?
  , ("er"  , [RightElEr        ]) -- most probably with RightCross
  ]

data Key
  = LeftGKGe
  | LeftBPBe
  | LeftCrossPrim
  | LeftH
  | LeftSChSchZTschTs
  | LeftJ
  | LeftFVW
  | LeftDDeDiTTe
  | LeftM
  | LeftL
  | LeftN
  | LeftREr
  | LeftCrossSnd
  | LeftE
  | LeftA
  | LeftO
  | RightPointSnd
  | RightUUmlautY
  | RightI
  | RightU
  | RightPointPrim
  | RightRRe
  | RightCross
  | RightL
  | RightGKCkCh
  | RightBPBePe
  | RightFVW
  | RightSChSchZTsTschTzEs
  | RightNMEm
  | RightDTTe
  | RightEn
  | RightElEr
  deriving (Eq, Ord)

instance TextShow Key where
  showb = \case
    LeftGKGe               -> "G"
    LeftBPBe               -> "B"
    LeftCrossPrim          -> "+"
    LeftH                  -> "H"
    LeftSChSchZTschTs      -> "S"
    LeftJ                  -> "J"
    LeftFVW                -> "F"
    LeftDDeDiTTe           -> "D"
    LeftM                  -> "M"
    LeftL                  -> "L"
    LeftN                  -> "N"
    LeftREr                -> "R"
    LeftCrossSnd           -> "*"
    LeftE                  -> "E"
    LeftA                  -> "A"
    LeftO                  -> "O"
    RightPointSnd          -> "°"
    RightUUmlautY          -> "Ü"
    RightI                 -> "I"
    RightU                 -> "U"
    RightPointPrim         -> "^"
    RightRRe               -> "R"
    RightCross             -> "+"
    RightL                 -> "L"
    RightGKCkCh            -> "G"
    RightBPBePe            -> "B"
    RightFVW               -> "F"
    RightSChSchZTsTschTzEs -> "Z"
    RightNMEm              -> "M"
    RightDTTe              -> "D"
    RightEn                -> "n"
    RightElEr              -> "l"

data Finger
  = FingerNone
  | LeftPinky
  | LeftRing
  | LeftMiddle
  | LeftIndex
  | LeftThumb
  | RightThumb
  | RightIndex
  | RightMiddle
  | RightRing
  | RightPinky
  deriving (Eq, Ord)

finger :: Key -> Finger
finger = \case
  LeftGKGe               -> LeftPinky
  LeftBPBe               -> LeftPinky
  LeftCrossPrim          -> LeftPinky
  LeftH                  -> LeftRing
  LeftSChSchZTschTs      -> LeftRing
  LeftJ                  -> LeftRing
  LeftFVW                -> LeftMiddle
  LeftDDeDiTTe           -> LeftMiddle
  LeftM                  -> LeftMiddle
  LeftL                  -> LeftIndex
  LeftN                  -> LeftIndex
  LeftREr                -> LeftIndex
  LeftCrossSnd           -> LeftThumb
  LeftE                  -> LeftThumb
  LeftA                  -> LeftThumb
  LeftO                  -> LeftThumb
  RightPointSnd          -> RightThumb
  RightUUmlautY          -> RightThumb
  RightI                 -> RightThumb
  RightU                 -> RightThumb
  RightPointPrim         -> RightIndex
  RightRRe               -> RightIndex
  RightCross             -> RightIndex
  RightL                 -> RightMiddle
  RightGKCkCh            -> RightMiddle
  RightBPBePe            -> RightMiddle
  RightFVW               -> RightRing
  RightSChSchZTsTschTzEs -> RightRing
  RightNMEm              -> RightRing
  RightDTTe              -> RightPinky
  RightEn                -> RightPinky
  RightElEr              -> RightPinky


-- chord :: Parsec Text ParserState Chord
-- chord = inner <* (eof <|> char '|')
--   where
--     inner = many1 key
--     key =

-- probably useless:

newtype Part = Part { unPart :: Text }

parseParts
  :: Text
  -> Either ParseError [Part]
parseParts = parse parts "command line input"
  where
    parts = sepBy1 part $ char '|'
    part = Part . Text.pack <$> letters
    letters = many1 letter
