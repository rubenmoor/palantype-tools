{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Text.Parsec (Parsec, sepBy1, parse, many1, letter, char, ParseError, eof)
import qualified Data.Text as Text
import Control.Applicative (Alternative((<|>)))

main :: IO ()
main = putStrLn "Hello, Haskell!"

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

data Finger
  = LeftPinky
  | LeftRing
  | LeftMiddle
  | LeftIndex
  | LeftThumb
  | RightThumb
  | RightIndex
  | RightMiddle
  | RightRing
  | RightPinky

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


newtype Chord = Chord { unChord :: [Key] }

data ParserState = ParserState
  { pstScore :: Int
  , pstKeys  :: [Key]
  }

primitives :: [(Text, [Key])]
primitives =
  [ ("g"   , [LeftGKGe         ])
  , ("k"   , [LeftGKGe         ])
  , ("ge"  , [LeftGKGe         ])
  , ("b"   , [LeftBPBe         ])
  , ("p"   , [LeftBPBe         ])
  , ("be"  , [LeftBPBe         ])
  , ("h"   , [LeftH            ])
  , ("s"   , [LeftSChSchZTschTs])
  , ("ß"   , [LeftSChSchZTschTs])
  , ("c"   , [LeftSChSchZTschTs])
  , ("ch"  , [LeftSChSchZTschTs])
  , ("sch" , [LeftSChSchZTschTs])
  , ("z"   , [LeftSChSchZTschTs])
  , ("tsch", [LeftSChSchZTschTs])
  , ("ts"  , [LeftSChSchZTschTs])
  , ("j"   , [LeftJ            ])
  , ("je"  , [LeftJ            ])
  , ("y"   , [LeftJ            ])
  , ("f"   , [LeftFVW          ])
  , ("v"   , [LeftFVW          ])
  , ("w"   , [LeftFVW          ])
  , ("d"   , [LeftDDeDiTTe     ])
  , ("de"  , [LeftDDeDiTTe     ])
  , ("di"  , [LeftDDeDiTTe     ]) -- risky?
  , ("t"   , [LeftDDeDiTTe     ])
  , ("te"  , [LeftDDeDiTTe     ])
  , ("m"   , [LeftM            ])
  , ("l"   , [LeftL            ])
  , ("n"   , [LeftN            ])
  , ("en"  , [LeftN            ])
  , ("r"   , [LeftREr          ])
  , ("er"  , [LeftREr          ])
  , ("e"   , [LeftE            ])
  , ("ä"   , [LeftE            ])
  , ("a"   , [LeftA            ])
  , ("o"   , [LeftO            ])
  , ("ü"   , [RightUUmlautY    ])
  , ("y"   , [RightUUmlautY    ])
  , ("i"   , [RightI           ])
  , ("u"   , [RightU           ])
  , ("r"   , [RightRRe         ])
  , ("rr"  , [RightRRe         ])
  , ("l"   , [RightL           ])
  , ("ll"  , [RightL           ])
  , ("g"   , [RightGKCkCh      ])
  , ("k"   , [RightGKCkCh      ])
  , ("ck"  , [RightGKCkCh      ])
  -- for ch -> RightGKCkCh, see "chs"
  , ("b"   , [RightBPBePe      ])
  , ("p"   , [RightBPBePe      ])
  , ("be"  , [RightBPBePe      ]) -- useful?
  , ("pe"  , [RightBPBePe      ]) -- useful?
  , ("pp"  , [RightBPBePe      ])
  , ("f"   , [RightFVW         ])
  , ("ff"  , [RightFVW         ])
  , ("v"   , [RightFVW         ])
  , ("w"   , [RightFVW         ])
  , ("ß"   , [RightSChSchZTsTschTzEs])
  , ("s"   , [RightSChSchZTsTschTzEs])
  , ("ss"  , [RightSChSchZTsTschTzEs])
  , ("z"   , [RightSChSchZTsTschTzEs])
  , ("ch"  , [RightSChSchZTsTschTzEs])
  , ("sch" , [RightSChSchZTsTschTzEs])
  , ("ts"  , [RightSChSchZTsTschTzEs])
  , ("tsch", [RightSChSchZTsTschTzEs])
  , ("tz"  , [RightSChSchZTsTschTzEs])
  , ("es"  , [RightSChSchZTsTschTzEs])
  , ("n"   , [RightNMEm        ])
  , ("nn"  , [RightNMEm        ])
  , ("m"   , [RightNMEm        ])
  , ("mm"  , [RightNMEm        ])
  , ("em"  , [RightNMEm        ])
  , ("d"   , [RightDTTe        ])
  , ("de"  , [RightDTTe        ])
  , ("dde" , [RightDTTe        ]) -- useful? risky?
  , ("t"   , [RightDTTe        ])
  , ("te"  , [RightDTTe        ])
  , ("tt"  , [RightDTTe        ])
  , ("en"  , [RightEn          ])
  , ("el"  , [RightElEr        ])
  , ("l"  , [RightElEr         ]) -- maybe for Austria?
  , ("er"  , [RightElEr        ]) -- most probably with RightCross
  , ("chs" , [RightGKCkCh, RightSChSchZTsTschTzEs])
  , ("qu"  , [LeftGKGe, LeftFVW])
  -- primitive flips
  , ("ng"  , [RightGKCkCh, RightNMEm]) -- ng -> GN
  , ("lk"  , [RightGKCkCh, RightElEr]) -- lk -> KL
  ]

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
