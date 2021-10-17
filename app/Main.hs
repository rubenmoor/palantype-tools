{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative (Alternative ((<|>)),
                                      Applicative (pure, (*>), (<*>)))
import           Control.Category    (Category ((.)))
import           Control.Monad       (MonadPlus (mzero), foldM)
import           Data.Bool           (Bool (False))
import           Data.Char           (Char)
import qualified Data.ByteString as ByteString
import           Data.Either         (Either (..))
import           Data.Eq             (Eq)
import           Data.Foldable       (Foldable (foldl, length, maximum),
                                      maximumBy)
import           Data.Function       (($))
import           Data.Functor        (Functor (fmap), void, ($>), (<$>), (<&>))
import Data.HashMap.Strict (HashMap)
import           Data.Int            (Int)
import           Data.List           (concat, dropWhile, head, intersperse,
                                      last, splitAt, (!!), (++))
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (Maybe (..), fromMaybe, maybe)
import           Data.Monoid         (Monoid (mconcat), (<>))
import           Data.Ord            (Ord ((>=)), comparing)
import           Data.Text           (Text, intercalate, splitOn, toLower)
import qualified Data.Text           as Text
import           Data.Text.IO        (interact, putStrLn)
import           Data.Traversable    (for)
import           Data.Tuple          (fst, snd)
import           GHC.Float           (Double)
import           GHC.Num             (Num ((-)), (+))
import           GHC.Real            (Fractional ((/)), fromIntegral, (^))
import           Safe                (headMay, lastMay)
import           System.Environment  (getArgs)
import           System.IO           (IO)
import           Text.Parsec         (ParseError, Parsec, char, eof, getState,
                                      letter, many, many1, parse, runParser,
                                      sepBy1, setState, string, try)
import           Text.Show           (Show (show))
import           TextShow            (TextShow (showb, showt), singleton)
import qualified Data.HashMap.Strict as HasMap
import Data.FileEmbed (embedFile)
import qualified Data.ByteString.Char8 as Char8
import qualified Text.JSON5 as JSON5
import GHC.Err (error)
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Data.Bifunctor (Bifunctor(second))
import Data.String (String)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ parseSeries $ Text.pack $ head args

parseSeries str =
  case optSeries $ splitOn "|" $ toLower str of
        Right (scoreAcc, chords) ->
          let -- the score of a series of chords for a given word is the average
              -- chord score, i.e. the average number of letters per chord
              score = (fromIntegral scoreAcc :: Double)
                    / fromIntegral ( length chords)
          in  showt (Series chords) <> " " <> showt score
        Left  err   -> err

optSeries :: [Text] -> Either Text (Int, [Chord])
optSeries [] = Right (0, [])
optSeries parts =
  maximum $
    ([1 .. (length parts)] :: NonEmpty Int) <&> \i ->
      let (front, back) = splitAt i parts
      in  do
            (scoreFront, cFront) <- parseChord $ intercalate "|" front
            (scoreBack , cBack ) <- optSeries back
            pure (scoreFront + scoreBack, cFront : cBack)

parseChord :: Text -> Either Text (Int, Chord)
parseChord str =
    case runParser chord FingerNone "series" str of
      Left  err -> Left  $ Text.pack $ show err
      -- the score of a chord is the number of letters it successfully encoded
      Right ks  -> Right (Text.length str, Chord ks)
  where
    chord = do
      initial <- keys
      tail <- concat <$> many (pipe <|> keys)
      eof
      pure $ initial ++ tail

    pipe = char '|' $> []

    keys = do

      lastFinger <- getState

      -- drop used keys from list
      -- a key is used when the finger of that key has been used
      let predicate pair = fromMaybe False $ do
            k <- headMay $ snd pair
            pure $ lastFinger >= finger k

          remPrimitives = dropWhile predicate primitives

          -- string consumes when fails
          acc parser (str, ks) =
            let primitive = do
                  _ <- try $ string $ Text.unpack str
                  setState $ maybe lastFinger finger (lastMay ks)
                  pure ks
            in  parser <|> primitive
      foldl acc mzero remPrimitives

-- a series of chords, to type a word of arbitrary length
newtype Series = Series { unSeries :: [Chord] }
  deriving (Eq, Ord)

instance TextShow Series where
  showb = mconcat . intersperse (singleton '/') . fmap showb . unSeries

newtype Chord = Chord { unChord :: [Key] }
  deriving (Eq, Ord)

instance TextShow Chord where
  showb = mconcat . fmap showb . unChord

-- full word exceptions
-- exceptions that span several chords go here
mapWordExceptions :: HashMap Text Text
mapWordExceptions = HasMap.fromList
  [ ("archaisch", "AO/SJAIʃ")
  , ("Chassis"  , "SJAS/SIS")
  , ("charmant" , "SJAO/MAND")
  , ("chopin"   , "SJO/BDIN")
  ]

-- primitives

mapCharParsers :: Trie [Text]
mapCharParsers =
  let str = Char8.unpack $(embedFile "primitives.json5")
      ls = case JSON5.decodeStrict str of
             JSON5.Ok m      -> m
             JSON5.Error err -> error $ "Could not read primitives.json5: " <> err
  in  Trie.fromList ls

-- the 'code' is raw steno written out
mapCodeKey :: Map Char [Key]
mapCodeKey =
  [ ('S', [LeftSZ, RightSZTz])
  , ('B', [LeftBP])
  , ('G', [LeftGK])
  , ('H', [LeftHE])
  , ('D', [LeftDT, RightDT])
  , ('F', [LeftFV, RightFWVIv])
  , ('M', [LeftM, RightM])
  , ('J', [LeftJ])
  , ('W', [LeftW])
  , ('L', [LeftL, RightL])
  , ('N', [LeftN, RightN])
  , ('R', [LeftREr])
  , ('^', [LeftDiacritic])
  , ('E', [LeftE])
  , ('A', [LeftA])
  , ('~', [LeftStretch])
  , ('O', [RightOStretch])
  , ('I', [RightI])
  , ('U', [RightU])
  , ('+', [RightModifier])
  , ('K', [RightKG])
  , ('P', [RightPB])
  , ('ʃ', [RightSchCh])
  , ('e', [RightEEr])
  , ('n', [RightEn])
  ]

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
  , ("x"   , [LeftGKGe, LeftSChSchZTschTs])
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
  , ("chaise", [LeftCrossPrim, LeftSChSchZTschTs, LeftE, RightPointSnd, RightCross, RightSChSchZTsTschTz])
  , ("chau", [LeftSChSchZTschTs, LeftA, RightU])
  , ("champ",[LeftSChSchZTschTs, LeftE, RightNMEm])
  , ("cham", [LeftSChSchZTschTs, LeftA, RightPointSnd, RightNMEm])
  , ("chan", [LeftSChSchZTschTs, LeftA, RightPointSnd, RightNMEm])
  , ("charg",[LeftSChSchZTschTs, LeftA, RightRRe, RightSChSchZTsTschTz])
  , ("cha" , [LeftGKGe         , LeftA])
  -- TODO: exceptions. pronounciation of "char" depends on word
  -- archaisch, charmant
  -- TODO: exception: chassis: full word exception
  , ("cho" , [LeftGKGe, LeftO])
  , ("chö" , [LeftGKGe, LeftO, RightPointSnd])
  , ("chr" , [LeftGKGe, LeftREr])
  , ("ch"  , [LeftSChSchZTschTs])
  , ("ci"  , [LeftSChSchZTschTs, RightI])
  , ("ck"  , [LeftGKGe])
  , ("c"   , [LeftGKGe])

  , ("ü"   , [RightUUmlautY    ])
  , ("y"   , [RightUUmlautY    ])
  , ("ie"  , [RightI           ])
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
                                  -- TODO: exceptions: archaisch, charmant, chassis
  , ("pe"  , [RightBPBePe      ]) -- useful?
  , ("pp"  , [RightBPBePe      ])
  , ("p"   , [RightBPBePe      ])
  , ("ff"  , [RightFVW         ])
  , ("f"   , [RightFVW         ])
  , ("v"   , [RightFVW         ])
  , ("w"   , [RightFVW         ])
  , ("ß"   , [RightSChSchZTsTschTz])
  , ("sch" , [RightSChSchZTsTschTz])
  , ("ss"  , [RightSChSchZTsTschTz])
  , ("s"   , [RightSChSchZTsTschTz])
  , ("z"   , [RightSChSchZTsTschTz])
  , ("chs" , [RightGKCkCh, RightSChSchZTsTschTz]) -- phonetically ks, like in "Wachs"
  , ("ch"  , [RightSChSchZTsTschTz])
  , ("tsch", [RightSChSchZTsTschTz])
  , ("ts"  , [RightSChSchZTsTschTz])
  , ("tz"  , [RightSChSchZTsTschTz])
  , ("es"  , [RightSChSchZTsTschTz])
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
  , ("l"   , [RightElEr         ]) -- maybe for Austria?
  , ("er"  , [RightElEr        ]) -- most probably with RightCross
  , ("x"   , [RightGKCkCh, RightSChSchZTsTschTz ])
  , ("h"   , [])
  ]

-- a key on a steno keyboard
data Key
  = LeftSZ
  | LeftBP
  | LeftGK
  | LeftHE
  | LeftDT
  | LeftFV
  | LeftM
  | LeftJ
  | LeftW
  | LeftL
  | LeftN
  | LeftREr
  | LeftDiacritic
  | LeftE
  | LeftA
  | LeftStretch
  | RightOStretch
  | RightI
  | RightU
  | RightModifier
  | RightL
  | RightM
  | RightN
  | RightKG
  | RightPB
  | RightFWVIv
  | RightSZTz
  | RightSchCh
  | RightEEr
  | RightEn
  | RightDT
  deriving (Eq, Ord)

instance TextShow Key where
  showb = \case
    LeftSZ        -> "S"
    LeftBP        -> "B"
    LeftGK        -> "G"
    LeftHE        -> "H"
    LeftDT        -> "D"
    LeftFV        -> "F"
    LeftM         -> "M"
    LeftJ         -> "J"
    LeftW         -> "W"
    LeftL         -> "L"
    LeftN         -> "N"
    LeftREr       -> "R"
    LeftDiacritic -> "^"
    LeftE         -> "E"
    LeftA         -> "A"
    LeftStretch   -> "~"
    RightOStretch -> "O"
    RightI        -> "I"
    RightU        -> "U"
    RightModifier -> "+"
    RightL        -> "L"
    RightM        -> "M"
    RightN        -> "N"
    RightKG       -> "K"
    RightPB       -> "P"
    RightFWVIv    -> "F"
    RightSZTz     -> "S"
    RightSchCh    -> "ʃ" -- U+0283
    RightEEr      -> "e"
    RightEn       -> "n"
    RightDT       -> "D"

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
  deriving (Eq, Ord)

-- the palantype.de keyboard
--
-- S|Z   H/e  M  L       .     + N   F/W/V  R/-er
-- B|P   D|T  J  N       .     L K/G S/Z|Tz -en
-- G|K   F/V  W  R/er-   .     M P/B ʃ|ç    D/T
-- thumbg     ^  E  A ː  . O I U

finger :: Key -> Finger
finger = \case
  LeftSZ        -> LeftPinky
  LeftBP        -> LeftPinky
  LeftGK        -> LeftPinky
  LeftHE        -> LeftRing
  LeftDT        -> LeftRing
  LeftFV        -> LeftRing
  LeftM         -> LeftMiddle
  LeftJ         -> LeftMiddle
  LeftW         -> LeftMiddle
  LeftL         -> LeftIndex
  LeftN         -> LeftIndex
  LeftREr       -> LeftIndex
  LeftDiacritic -> LeftThumb
  LeftE         -> LeftThumb
  LeftA         -> LeftThumb
  LeftStretch   -> LeftThumb
  RightOStretch -> RightThumb
  RightI        -> RightThumb
  RightU        -> RightThumb
  RightModifier -> RightIndex
  RightL        -> RightIndex
  RightM        -> RightIndex
  RightN        -> RightMiddle
  RightKG       -> RightMiddle
  RightPB       -> RightMiddle
  RightFWVIv    -> RightRing
  RightSZTz     -> RightRing
  RightSchCh    -> RightRing
  RightEEr      -> RightPinky
  RightEn       -> RightPinky
  RightDT       -> RightPinky


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
