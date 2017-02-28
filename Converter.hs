#! /usr/bin/env stack
{- stack
    --resolver lts-7.14
    --install-ghc
    runghc
    --package yaml
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Aeson
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Ord
import Data.Maybe
import Data.List
import Control.Monad
import Control.Exception
import Data.Foldable
import System.FilePath
import System.Directory
import Data.Char
import Data.String
import Conduit

-- These definitions set which types will be processed and how they
-- will be handled.

data HaskAnythingType = Article | Paper | Video
    deriving (Eq, Show, Enum, Bounded)

instance ToJSON HaskAnythingType where
    toJSON = String . haTypeName

haTypeName :: HaskAnythingType -> Text
haTypeName = \case
    Article -> "article"
    Paper -> "paper"
    Video -> "presentation"

-- Changes the filing type of an entry. If you don't want some
-- DoHaskell to be handled, remove its entry or change it to
-- Nothing.
dhTypeToHa :: Text -> Maybe HaskAnythingType
dhTypeToHa = \case
    "Master's thesis" -> Just Paper
    "blog post" -> Just Article
    "dissertation" -> Just Paper
    "experience report" -> Just Article
    "extended example" -> Just Article
    "functional pearl" -> Just Paper -- Where to file these?
    "journal paper" -> Just Paper
    "light research paper" -> Just Paper -- And these?
    "research paper" -> Just Paper
    "survey article" -> Just Paper
    "video lecture" -> Just Video
    _ -> Nothing

primaryUrlField :: HaskAnythingType -> Text
primaryUrlField = \case
    Article -> "url"
    Paper -> "paper-url"
    Video -> "url-video"

data FieldSortKey = RecognisedFieldSortKey Int | UnrecognisedFieldSortKey Text
    deriving (Eq, Ord, Show)

fieldSortKey :: Text -> FieldSortKey
fieldSortKey field = fromMaybe (UnrecognisedFieldSortKey field) $
    M.lookup field recognisedFieldMap
    where
    recognisedFieldMap = M.fromList $ zip
        [ "title"
        , "url"
        , "paper-url"
        , "url-video"
        , "url-slides"
        , "authors"
        , "type"
        , "tags"
        , "libraries"
        ]
        (RecognisedFieldSortKey <$> [0..])

-- Unifies duplicated tags and fixes mispellings.
-- Switching to Text -> [Text] might be useful for filling entries in
-- two tags at once (e.g. a regular tag and a library one).
aliasTag :: Text -> Text
aliasTag = \case
    "Cabal" -> "cabal"
    "Logic monad" -> "LogicT"
    "Profunctor" -> "profunctors"
    "catamorphisms" -> "recursion schemes"
    "monadplus" -> "MonadPlus"
    "Parsec" -> "parsec"
    "singleton types" -> "singletons"
    "web sockets" -> "websockets"
    tag -> tag

-- Which tags should be moved to the libraries field.
-- Commented out entries are things that may or may not count as
-- "libraries", depending on the intended meaning of the field.
-- Note that this is ran *after* aliasTag is used.
libraryTags :: Set Text
libraryTags = S.fromList
    [ "vector"
    --, "Fay"
    --, "GHCJS"
    , "Haxl"
    , "Happstack"
    --, "MongoDB"
    , "MVC" -- The Gabriel Gonzalez library, not the broader concept.
    --, "PostgreSQL"
    , "QML"
    , "QT"
    , "QuickCheck"
    , "Shake"
    , "Snap"
    , "Yesod"
    --, "Yi"
    , "aeson"
    , "auto"
    , "attoparsec"
    --, "cabal"
    , "conduit"
    , "criterion"
    , "diagrams"
    , "digestive-functors"
    , "esqueleto"
    , "gloss"
    , "hakyll"
    , "hoopl"
    , "hspec"
    --, "lenses" -- *Most* entries are about lens.
    --, "liquidhaskell"
    , "machines"
    , "operational"
    , "parsec"
    , "persistent"
    , "pipes"
    --, "reflection" -- One entry is about the homonymous package.
    , "repa"
    , "scotty"
    --, "singletons"
    --, "template haskell"
    , "time"
    , "turtle"
    --, "websockets" -- Slightly ambiguous tag.
    ]

-- Intermediate representations and their aeson instances.

(.=++) :: (ToJSON v, KeyValue t) => Text -> v -> [t]
k .=++ v = [ k .= v ]

(.=++?) :: (ToJSON v, KeyValue t) => Text -> Maybe v -> [t]
k .=++? v = (k .=) <$> maybeToList v

data DoHaskellEntry = DoHaskellEntry
    { dhTitle :: Text
    , dhUrl :: Text
    , dhAuthors :: Maybe (Vector Text)
    , dhPublished :: Maybe Int
    , dhType :: Text
    , dhTags :: Vector Text
    , dhCollections :: Maybe (Vector Text)
    } deriving (Show)

instance FromJSON DoHaskellEntry where
    parseJSON (Object v) = DoHaskellEntry
        <$> v .: "title"
        <*> v .: "url"
        <*> (v .:? "authors")
        <*> v .:? "published"
        <*> v .: "type"
        <*> v .: "tags"
        <*> (v .:? "collections")

instance ToJSON DoHaskellEntry where
    toJSON dhEn = object . concat $
        [ "title" .=++ dhTitle dhEn
        , "url" .=++ dhUrl dhEn
        , "authors" .=++? dhAuthors dhEn
        , "published" .=++? dhPublished dhEn
        , "type" .=++ dhType dhEn
        , "tags" .=++ dhTags dhEn
        , "collections" .=++? dhCollections dhEn
        ]

data HaskAnythingEntry = HaskAnythingEntry
    { haType :: HaskAnythingType
    , haDhEntry :: DoHaskellEntry
    } deriving (Show)

instance ToJSON HaskAnythingEntry where
    toJSON haEn = object . concat $
        [ "title" .=++ dhTitle dhEn
        , primaryUrlField haTy .=++ dhUrl dhEn
        , "authors" .=++? dhAuthors dhEn
        , "type" .=++ haTy
        , "tags" .=++? if null tags then Nothing else Just tags
        , "libraries" .=++? if null libs then Nothing else Just libs
        , "doHaskell-type" .=++ dhType dhEn
        , "dohaskell-year" .=++? dhPublished dhEn
        , "dohaskell-collections" .=++? dhCollections dhEn
        ]
        where
        dhEn = haDhEntry haEn
        haTy = haType haEn
        (libs, tags) = V.partition (`S.member` libraryTags) (dhTags dhEn)

dhEntryToHa :: DoHaskellEntry -> Maybe HaskAnythingEntry
dhEntryToHa dhEn = case dhTypeToHa (dhType dhEn) of
    Nothing -> Nothing
    Just haTy -> Just (HaskAnythingEntry haTy dhEn)

-- Mangling titles for creating the .md filenames.
-- Note that I haven't sanity checked this at all.
mangleTitle :: Text -> Text
mangleTitle  = T.replace " " "-" . T.filter (\c -> isAlphaNum c || c == ' ')

-- The converter. If you don't want the output directory to be deleted
-- between runs, just create an extra file in the output directory.
main = do
    paranoidDeleteOutputDir
    eDoHask <- readDoHaskellDump (workDir </> "resources-dump-wip.yaml")
    case eDoHask of
        Left err -> throwIO err
        Right doHask -> do
            -- writeTagsAndTypes workDir doHask
            traverse_ (createDirectoryIfMissing True) outputSubDirs
            runResourceT $ runConduit $
                yieldMany doHask
                .| applyTagAliases
                .| genNewYaml
                .| sinkOldYaml
    where
    workDir = "."
    outputDir = workDir </> "ha-dh-output"
    outputSubDirNames = T.unpack . haTypeName <$> [minBound..maxBound]
    outputSubDirs = (outputDir </>) <$> outputSubDirNames
    leftoversFileName = "leftovers.yaml"

    paranoidDeleteOutputDir = do
        oldOutputExists <- doesDirectoryExist outputDir
        when oldOutputExists $ do
            contents <- S.fromList <$> listDirectory outputDir
            let expected = S.fromList (leftoversFileName : outputSubDirNames)
            when (contents == expected) $
                removeDirectoryRecursive outputDir

    applyTagAliases = mapC $ \en -> en{ dhTags = aliasTag <$> dhTags en }

    genNewYaml = awaitForever $ \dhEn ->
        case dhEntryToHa dhEn of
            Nothing -> do
                yield (Y.encode dhEn)
                yield "\n"
            Just haEn -> yieldMd haEn .| sinkMd haEn

    yieldMd haEn = do
        yield "---\n"
        yield (Y.encodePretty sortedFieldConfig haEn)
        yield "---"
    sortedFieldConfig = Y.setConfCompare (comparing fieldSortKey) Y.defConfig

    sinkMd haEn = sinkFile (mdPath haEn)
    mdPath haEn = outputDir
        </> T.unpack (haTypeName (haType haEn))
        </> T.unpack (mangleTitle (dhTitle (haDhEntry haEn))) <.> ".md"

    sinkOldYaml = sinkFile (outputDir </> leftoversFileName)

-- Helpers for exploring the data.

readDoHaskellDump :: FilePath -> IO (Either Y.ParseException [DoHaskellEntry])
readDoHaskellDump inpFile = Y.decodeFileEither inpFile

allDhTypes :: [DoHaskellEntry] -> Set Text
allDhTypes = foldr (S.insert . dhType) S.empty

allDhTags :: [DoHaskellEntry] -> Set Text
allDhTags = foldr ((\ts s -> V.foldl' (flip S.insert) s ts) . dhTags) S.empty

writeTagsAndTypes :: FilePath -> [DoHaskellEntry] -> IO ()
writeTagsAndTypes destDir doHask = do
    T.writeFile (destDir </> "dhTypes.txt")
        . T.unlines . toList . allDhTypes $ doHask
    T.writeFile (destDir </> "dhTags.txt")
        . T.unlines . toList . allDhTags $ doHask

