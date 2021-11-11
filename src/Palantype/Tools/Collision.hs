module Palantype.Tools.Collision where

import Data.Text (Text)
import System.IO (IO, FilePath)
import Data.Int (Int)
import Control.Applicative (Applicative(pure), (<$>))
import Data.HashMap.Internal.Strict (HashMap)
import Data.Text.Lazy.IO (readFile, appendFile)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Text.Read (read)
import Data.Function (($))
import Data.Semigroup ((<>))
import GHC.Err (error)
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (Maybe(..))
import Network.Wreq (responseStatus, statusCode, responseBody, defaults)
import Control.Category (Category((.)))
import Control.Lens.Getter ((^.))
import Data.Aeson.Lens (key, _Integral)
import Control.Lens ((^?))
import TextShow (TextShow(showt))
import Network.Wreq.Session (Session, newAPISession, getWith)
import Control.Lens.Setter (set)
import Network.Wreq.Lens (checkResponse)

-- | unfortunately, UNI Leipzig doesn't provide any corpus bigger then
--   1 Mio. sentences
corpusName :: Text
corpusName = "deu_news_2012_1M"

fileFrequencyCache :: FilePath
fileFrequencyCache = "frequencies.txt"

fileFrequencyErrors :: FilePath
fileFrequencyErrors = "frequencies-errors.txt"

readFrequencies :: IO (HashMap Text Int)
readFrequencies = do
  ls <- Lazy.lines <$> readFile fileFrequencyCache
  let
    acc m l = case Lazy.splitOn " " l of
      [w, strFrequency] ->
        HashMap.insert (Lazy.toStrict w) (read $ Lazy.unpack strFrequency) m
      _ -> error $ "could not read: " <> Lazy.unpack l
  pure $ foldl' acc HashMap.empty ls

getSession :: IO Session
getSession = newAPISession

lookUpFrequency
  :: HashMap Text Int
  -> Session
  -> Text
  -> IO Int
lookUpFrequency cache s w =
  case HashMap.lookup w cache of
    Just f -> pure f
    Nothing -> do
      r <- getWith reqOpts s $ Text.unpack $
        "http://api.corpora.uni-leipzig.de/ws/words/"
        <> corpusName
        <> "/word/" <> w
      let
        (f, mError) = case r ^. responseStatus . statusCode of
          200 -> case r ^? responseBody . key "freq" . _Integral of
            Nothing -> (0, Just "freq missing")
            Just n  -> (n, Nothing)
          404 -> (0, Just "no entry in corpus")
          other -> (0, Just $ "Other error: " <> showt other)
      case mError of
        Just str -> appendFile fileFrequencyErrors $
          Lazy.fromStrict $ w <> ": " <> str <> "\n"
        Nothing -> pure ()
      appendFile fileFrequencyCache $ Lazy.fromStrict $
        w <> " " <> showt f <> "\n"
      pure f
  where
    reqOpts = set checkResponse (Just $ \_ _ -> pure ()) defaults

--       let
--         acc' m (r, pairs) = case pairs of
--
--           -- no collision: ignore path, store raw steno with word
--           -- TODO: unless there is already an entry
--           [(_, word)] -> pure $ HashMap.insert r word m
--
--           -- collision
--           p:_ -> do
--             mSolution <- solveCollision (snd <$> pairs) swsMapWordSteno
--             case mSolution of
--               Just stenoWord -> pure stenoWord
--               Nothing -> do
--                 appendLine fileStenoWordsCollisions $
--                   Lazy.fromStrict $ showt r <> ": " <> showt pairs
--                 pure (r, snd p)
--
--           -- impossible case
--           []     -> error "empty entry"
--
--       mapStenoWord <-
--         foldM acc' HashMap.empty (HashMap.toList swsMapStenoWords)
