{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language TypeSynonymInstances #-}

-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Post ( Post
            , Talk
            , archiveContext
            , talksContext
            , body
            , context
            , date
            , feedContext
            , talksFeedContext
            , longDate
            , parsePost
            , parseTalk
            , relatedContext
            , shortDate
            , selectRelated
            , slug
            , title
            , url
            , year ) where

import           Control.Lens
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.Text as Text
import           Data.Time.Format
import           Data.Time.Calendar (Day, showGregorian, toGregorian)
import           GHC.Exts (groupWith, sortWith)
import           Text.Pandoc

import qualified Html
import qualified Template
import qualified Type

-- Front matter consists of key value pairs, both of type string.
-- There is no fancy YAML here.
type FrontMatter = M.Map String String

-- Strips off and parses front matter from the string. Front matter is
-- delimited by triple dashes. Keys are anything before ": ", the value
-- is what comes after that. Ignores first line assuming it is "---".
extractFrontMatter :: String -> (FrontMatter, String)
extractFrontMatter = parseFM M.empty . drop 1 . lines
  where parseFM _ []                = error "Post should not be empty."
        parseFM fm ("---":postBody) = (fm, unlines postBody)
        parseFM fm (line:more)      = parseFM (M.insert key value fm) more
          where (key, delimValue) = break (== ':') line
                value = drop 2 delimValue -- Drop the colon and space.

data Piece a = Piece { title     :: String
                     , header    :: String
                     , subheader :: Maybe String
                     , part      :: Maybe Int
                     , date      :: Day
                     , slug      :: String
                     , _specific :: a  -- TODO: Remove if don't need to hold info in {Post,Talk}Specific.
                     , synopsis  :: String
                     , body      :: String } deriving (Show) -- TODO: This is for debugging only, remove.

data PostSpecific = Post
  deriving Show
data TalkSpecific = Talk
  deriving Show

type Post = Piece PostSpecific
type Talk = Piece TalkSpecific

-- makeClassy ''PostSpecific
-- makeClassy ''TalkSpecific
makeLenses ''Piece

-- instance HasPostSpecific Post where
--     postSpecific = specific

-- instance HasTalkSpecific Talk where
--     talkSpecific = specific

class IsPieceType p where
  urlPrefix :: p -> String

instance IsPieceType PostSpecific where
  urlPrefix _ = "/writing/"

instance IsPieceType TalkSpecific where
  urlPrefix _ = "/talks/"

-- Returns the post date, formatted like "April 17, 2015".
longDate :: Piece p -> String
longDate = formatTime defaultTimeLocale "%B %e, %Y" . date

-- Returns the post date, formatted like "2015-04-17".
shortDate :: Piece p -> String
shortDate = showGregorian . date

-- Returns the year in which the post was published.
year :: Piece a -> Integer
year piece = y where (y, _m, _d) = toGregorian $ date piece

-- Returns the canonical absolute url for a particular post.
url :: (IsPieceType p) => Piece p -> String
url p = urlPrefix (p^.specific) ++ slug p

-- Returns whether post has code in it that requires a monospace font.
usesMonoFont :: Piece p -> Bool
usesMonoFont = not . null . Html.filterTags Html.isCode . Html.parseTags . body

-- Returns whether the post has <em> tags that require an italic font.
usesItalicFont :: Piece p -> Bool
usesItalicFont = not . null . Html.filterTags Html.isEm . Html.parseTags . body

-- Converts an integer to a Roman numeral (nothing fancy, works for 1-9).
toRoman :: Int -> String
toRoman i = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] !! (i - 1)

-- Maps a boolean to a field that can be used in a template expansion context.
booleanField :: Bool -> Maybe String
booleanField b = if b then Just "true" else Nothing

-- Given a string, and a substring consisting of two words, inserts a <br> tag
-- and a space between the two words in the original string.
addBreak :: String -> String -> String
addBreak between = Text.unpack . Text.replace textBetween broken . Text.pack
  where [brAfter, brBefore] = words between
        textBetween         = Text.pack between
        broken              = Text.pack $ brAfter ++ "<br> " ++ brBefore

-- Returns the template expansion context for the post.
context :: (IsPieceType p)
        => Piece p -> Template.Context
context p = fmap Template.StringValue ctx
  where ctx       = M.union fields (M.mapMaybe id optFields)
        fields    = M.fromList [ ("title", title p)
                               , ("header", header p)
                               , ("short-date", shortDate p)
                               , ("long-date", longDate p)
                               , ("url", url p)
                               , ("synopsis", synopsis p)
                               , ("synopsis-html", Type.makeAbbrs $ synopsis p)
                               , ("content", body p) ]
        optFields = M.fromList [ ("subheader", subheader p)
                               , ("part", fmap toRoman $ part p)
                               , ("bold-font", boldFontField)
                               , ("italic-font", italicFontField)
                               , ("math", mathField)
                               , ("mono-font", monoFontField)
                               , ("serif-italic-font", serifItalicFontField) ]
        usesSerifItalic      = (Type.usesSerifItalicFont $ body p) || (isJust $ subheader p)
        boldFontField        = booleanField $ Type.usesBoldFont $ body p
        italicFontField      = booleanField $ usesItalicFont p
        mathField            = booleanField $ Html.hasMath $ body p
        monoFontField        = booleanField $ usesMonoFont p
        serifItalicFontField = booleanField $ usesSerifItalic

-- Given a slug and the contents of the post file (markdown with front matter),
-- renders the body to html and parses the metadata.
parsePost :: String -> String -> Post
parsePost postSlug contents = let
  (frontMatter, bodyContents) = extractFrontMatter contents
  postTitle     = frontMatter M.! "title"
  postHeading   = fromMaybe postTitle $ M.lookup "header" frontMatter
  breakAt       = M.lookup "break" frontMatter
  brokenHeading = foldr addBreak postHeading breakAt
  runIn         = M.lookup "run-in" frontMatter
  addRunIn html = foldl Html.makeRunIn html (fmap length runIn)
  refineType    = addRunIn . Type.expandPunctuation . Type.makeAbbrs
  parseDate     = parseTimeOrError True defaultTimeLocale "%F"
  in Piece { title    = postTitle
          , header    = brokenHeading
          , subheader = M.lookup "subheader" frontMatter
          , part      = fmap read $ M.lookup "part" frontMatter
          , date      = parseDate $ frontMatter M.! "date"
          , slug      = postSlug
          , synopsis  = frontMatter M.! "synopsis"
          , body      = refineType $ Html.cleanTables $ renderMarkdown bodyContents
          , _specific = Post }

-- Given a slug and the contents of the talk file (markdown with front matter),
-- renders the body to html and parses the metadata.
parseTalk :: String -> String -> Talk
parseTalk postSlug contents = let
  (frontMatter, bodyContents) = extractFrontMatter contents
  postTitle     = frontMatter M.! "title"
  postHeading   = fromMaybe postTitle $ M.lookup "header" frontMatter
  breakAt       = M.lookup "break" frontMatter
  brokenHeading = foldr addBreak postHeading breakAt
  runIn         = M.lookup "run-in" frontMatter
  addRunIn html = foldl Html.makeRunIn html (fmap length runIn)
  refineType    = addRunIn . Type.expandPunctuation . Type.makeAbbrs
  parseDate     = parseTimeOrError True defaultTimeLocale "%F"
  in Piece { title    = postTitle
          , header    = brokenHeading
          , subheader = M.lookup "subheader" frontMatter
          , part      = fmap read $ M.lookup "part" frontMatter
          , date      = parseDate $ frontMatter M.! "date"
          , slug      = postSlug
          , synopsis  = frontMatter M.! "synopsis"
          , body      = refineType $ Html.cleanTables $ renderMarkdown bodyContents
          , _specific = Talk }

-- Renders markdown to html using Pandoc with my settings.
renderMarkdown :: String -> String
renderMarkdown md = case fmap (writeHtmlString wopt) (readMarkdown ropt md) of
  Right result -> result
  Left  _      -> "Failed to parse markdown."
  -- Enable backtick code blocks and accept tables.
  -- For output, enable syntax highlighting.
  where ropt = def { readerExtensions = S.insert Ext_backtick_code_blocks $
                                        S.insert Ext_raw_html $
                                        S.insert Ext_simple_tables $
                                        def }
        wopt = def { writerHighlight  = True }

-- Related content for a post or talk, for the further reading section in the footer.
data RelatedContent a = Further (Piece a)
                      | Series [Piece a]
                      deriving (Show) -- TODO: this is for debugging only, remove.

-- Returns the template expansion context for related content.
relatedContext :: IsPieceType a => RelatedContent a -> Template.Context
relatedContext related = case related of
  Further p -> Template.nestContext "further" $ context p
  Series ps -> Template.listField   "series" $ fmap context ps

-- Takes an (unordered) list of posts or talks and produces a list of posts together with
-- related content for that post or talk.
selectRelated :: [Piece a] -> [(Piece a, RelatedContent a)]
selectRelated pieces = fmap nextElsePrev prevPieceNext
  where -- Create chronological triples of (previous post, post, next post).
        chronological = sortWith date pieces
        prevPieces    = Nothing : (fmap Just chronological)
        nextPieces    = (drop 1 $ fmap Just chronological) ++ [Nothing]
        prevPieceNext = zip3 prevPieces chronological nextPieces

        -- Select the next post as "Further" content if there is one, otherwise
        -- take the previous post (which is assumed to exist in that case).
        nextElsePrev x = case x of
          (_, piece, Just next) -> (piece, Further next)
          (Just prev, piece, _) -> (piece, Further prev)
          _                     -> error "Each piece type (e.g. posts or talks) requires at least two pieces of that type."

-- Returns a context for a group of posts that share the same year.
archiveYearContext :: [Post] -> Template.Context
archiveYearContext posts = yearField `M.union` postsField
  where yearField     = Template.stringField "year" $ show $ year $ head $ posts
        chronological = sortWith date posts
        recentFirst   = reverse chronological
        postsField    = Template.listField "post" $ fmap context recentFirst

-- Returns a context for a group of talks that share the same year.
talksYearContext :: [Talk] -> Template.Context
talksYearContext talks = yearField `M.union` talksField
  where yearField     = Template.stringField "year" $ show $ year $ head $ talks
        chronological = sortWith date talks
        recentFirst   = reverse chronological
        talksField    = Template.listField "talk" $ fmap context recentFirst

-- Returns a contexts with a "archive-year" list where every year has a "posts"
-- lists.
archiveContext :: [Post] -> Template.Context
archiveContext posts  = Template.listField "archive-year" years
  where yearGroups    = groupWith year posts
        chronological = sortWith (year . head) yearGroups
        recentFirst   = reverse chronological
        years         = fmap archiveYearContext recentFirst

-- Returns a contexts with a "archive-year" list where every year has a "talks"
-- lists.
talksContext :: [Talk] -> Template.Context
talksContext talks  = Template.listField "archive-year" years
  where yearGroups    = groupWith year talks
        chronological = sortWith (year . head) yearGroups
        recentFirst   = reverse chronological
        years         = fmap talksYearContext recentFirst

-- Context for generating an atom feed for the 15 most recent posts.
feedContext :: [Post] -> Template.Context
feedContext posts = updatedField `M.union` postsField
  where chronological = sortWith date posts
        recentFirst   = take 15 $ reverse chronological
        updatedField  = Template.stringField "updated" $ shortDate $ head recentFirst
        postsField    = Template.listField "post" $ fmap context recentFirst

-- Context for generating an atom feed for the 15 most recent talks.
talksFeedContext :: [Talk] -> Template.Context
talksFeedContext talks = updatedField `M.union` talksField
  where chronological = sortWith date talks
        recentFirst   = take 15 $ reverse chronological
        updatedField  = Template.stringField "updated" $ shortDate $ head recentFirst
        talksField    = Template.listField "talk" $ fmap context recentFirst
