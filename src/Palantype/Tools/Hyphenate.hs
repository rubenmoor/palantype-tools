{-# LANGUAGE DerivingStrategies #-}
module Palantype.Tools.Hyphenate where

import           Data.Eq                        ( Eq ((==)) )
import           Data.Function                  ( ($) )
import           Data.List                      ( intersperse )
import           Data.Monoid                    ( Monoid(mconcat), (<>) )
import           Data.Text                      ( Text )
import           TextShow                       ( TextShow(showb)
                                                , fromText
                                                )
import Control.Category ((<<<))
import Data.String (String)
import qualified Data.Text as Text
import Data.Either (Either(Left, Right))
import Text.Parsec (runParser, Parsec, try, (<|>), many1, many, getState, setState, char)
import Data.Char (Char, isLower)
import Data.Functor ((<$>))
import Control.Applicative (Applicative(pure))
import Control.Monad (guard, Monad ((>>=)), MonadPlus (mzero))
import Data.Foldable (all, Foldable (elem, foldr), notElem)
import Data.Bool ((&&))

newtype Hyphenated = Hyphenated { unHyphenated :: [Text] }
  deriving stock Eq

instance TextShow Hyphenated where
    showb (Hyphenated txts) = fromText (mconcat $ intersperse "|" txts)

toWord :: Hyphenated -> Text
toWord = mconcat <<< unHyphenated

hyphPseudoSyllable :: [String] -> [Text]
hyphPseudoSyllable = foldr accSylls []
  where
    accSylls oldSyl new =
      case runParser pseudoSyllable oldSyl "" (Text.pack oldSyl) of
        Left _ -> [Text.replace "eie" "ei|e" $ Text.pack oldSyl] <> new
        Right sylls -> sylls <> new

pseudoSyllable :: Parsec Text String [Text]
pseudoSyllable = do
    v1 <- Text.singleton <$> vowel
    (v1 :) <$> (try bmio <|> pseudoSyllable')
  where
    pseudoSyllable' = do
        c1  <- Text.pack <$> (pure <$> next 'y' <|> many1 lcConsonantWOY)
        v2  <- vowel
        guard (isLower v2)
        rem <- many nextChar
        guard $ all isLower rem
        pure [c1 <> Text.singleton v2 <> Text.pack rem]

bmio :: Parsec Text String [Text]
bmio = do
    bm <- next 'b' <|> next 'm'
    i  <- next 'i'
    o  <- next 'o'
    pure [Text.pack [bm, i], Text.singleton o]

nextChar :: Parsec Text String Char
nextChar = getState >>= \case
    (x : xs) -> do
        setState xs
        char x
    _ -> mzero

next :: Char -> Parsec Text String Char
next c = getState >>= \case
    (x : xs) | x == c -> do
        setState xs
        char x
    _ -> mzero

vowel :: Parsec Text String Char
vowel = getState >>= \case
    (x : xs) | x `elem` vowelsWY -> do
        setState xs
        char x
    _ -> mzero

lcConsonantWithY :: Parsec Text String Char
lcConsonantWithY = getState >>= \case
    (x : xs) | isLower x && x `notElem` vowels -> do
        setState xs
        char x
    _ -> mzero

lcConsonantWOY :: Parsec Text String Char
lcConsonantWOY = getState >>= \case
    (x : xs) | isLower x && x `notElem` vowelsWY -> do
        setState xs
        char x
    _ -> mzero

vowels :: String
vowels = "AEIOUÄÖÜÁÀÂÅÉÈÊÍÓÔÚaeiouäöüáàâåéèêëíóôøû"

vowelsWY :: String
vowelsWY = 'y' : 'Y' : vowels
