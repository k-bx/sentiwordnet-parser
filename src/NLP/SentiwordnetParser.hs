{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module NLP.SentiwordnetParser
  ( parse
  , SentiWordNet(..)
  , Entry(..)
  , SynsetTerm(..)
  , POS(..)
  , SentiWordNetLookup
  , SentiWordNetLookupItem(..)
  , toSentiWordNetLookup
  -- * internal stuff
  , parseSentiWordNet
  , parsePOS
  , parseDecimal
  , parseInt
  , parseEntry
  , parseSynsetTerm
  , parseComment
  , test
  ) where

import Control.Applicative
import Data.Decimal (Decimal)
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Safe
import Text.Trifecta

data SynsetTerm = SynsetTerm
  { name :: Text
  , num :: Int
  } deriving (Show, Eq)

data POS
  = Noun
  | Verb
  | Adjective
  | AdjectiveSatellite
  | Adverb
  deriving (Show, Eq, Generic, Hashable)

data Entry = Entry
  { pos :: POS
  , id_ :: Text
  , posScore :: Decimal
  , negScore :: Decimal
  , synsetTerms :: [SynsetTerm]
  , gloss :: Text
  } deriving (Show, Eq)

data SentiWordNet = SentiWordNet
  { items :: [Entry]
  } deriving (Show, Eq)

parsePOS :: Parser POS
parsePOS =
  pure Noun <* char 'n' <|> pure Verb <* char 'v' <|> pure Adjective <* char 'a' <|>
  pure AdjectiveSatellite <* char 's' <|>
  pure Adverb <* char 'r'

parseInt :: Parser Int
parseInt = do
  s <- many alphaNum
  case readMay s of
    Nothing -> fail ("Failed to read an int: " ++ s)
    Just res -> return res

parseDecimal :: Parser Decimal
parseDecimal = do
  s <- many (alphaNum <|> char '.')
  let r = readMay s
  case r of
    Nothing -> fail ("Failed to read decimal: " ++ s)
    Just res -> return res

parseSynsetTerm :: Parser SynsetTerm
parseSynsetTerm = do
  name <- T.pack <$> many (noneOf ['#'])
  _ <- char '#'
  num <- parseInt
  return SynsetTerm {..}

parseEntry :: Parser Entry
parseEntry = do
  _ <- optional (many parseComment)
  pos <- parsePOS
  _ <- tab
  id_ <- T.pack <$> many (noneOf ['\t'])
  _ <- tab
  posScore <- parseDecimal
  _ <- tab
  negScore <- parseDecimal
  _ <- tab
  synsetTerms <- sepBy parseSynsetTerm (char ' ')
  _ <- tab
  gloss <- T.pack <$> many (noneOf ['\r', '\n'])
  _ <- optional newline
  return (Entry {..})

parseComment :: Parser String
parseComment = char '#' *> many (noneOf ['\n']) <* optional newline

parseSentiWordNet :: Parser SentiWordNet
parseSentiWordNet = SentiWordNet <$> some parseEntry

parse :: Text -> Result SentiWordNet
parse = parseString parseSentiWordNet mempty . T.unpack

data SentiWordNetLookupItem = SentiWordNetLookupItem
  { lookPos :: Int
  , lookPosScore :: Decimal
  , lookNegScore :: Decimal
  }

-- | Datastructure for efficient 'lookupScoreByPosAndName' lookups
type SentiWordNetLookup = H.HashMap (POS, Text) [SentiWordNetLookupItem]

-- | Convert function
toSentiWordNetLookup :: SentiWordNet -> SentiWordNetLookup
toSentiWordNetLookup SentiWordNet {..} =
  H.fromListWith (<>) (concatMap convEntry items)
  where
    convEntry Entry {..} = map (convTerm pos posScore negScore) synsetTerms
    convTerm pos posScore negScore SynsetTerm {..} =
      ((pos, name), [SentiWordNetLookupItem num posScore negScore])

test :: IO ()
test = do
  sentiWordNet <-
    T.readFile
      "/home/kb/Downloads/SentiWordNet_3.0.0/SentiWordNet_3.0.0_20130122.txt"
  let res = parse sentiWordNet
  case res of
    Success r -> print (length (items r))
    Failure e -> print e
