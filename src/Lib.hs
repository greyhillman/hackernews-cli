{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
module Lib where

--import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import Network.Wreq
import Control.Lens
import Control.Monad
import Text.HTML.Scalpel


type Author = String
data Comment = Comment { commentAuthor :: Author
                       , commentChildren :: [Comment]
                       } deriving (Eq, Show)

data Post = Post { postTitle :: String
                 , postURL :: Maybe String
                 , postText :: Maybe String
                 , postAuthor :: Author
                 , postTime :: Integer
                 , postNumComments :: [Int]
                 } deriving (Eq, Show)

instance FromJSON Post where
    parseJSON (Object v) = Post <$>
        v .: "title" <*>
        v .:? "url" <*>
        v .:? "text" <*>
        v .: "by" <*>
        v .: "time" <*>
        v .:? "kids" .!= []
    parseJSON _ = mzero

groupBy2 :: [a] -> [(a, a)]
groupBy2 [] = []
groupBy2 (x:y:xs) = (x, y) : groupBy2 xs
groupBy2 (x:[]) = error "Not divisible by 2"

dropLast :: Int -> [a] -> [a]
dropLast x = reverse . drop x . reverse

scrapePost :: Scraper String String
scrapePost = do
    x <- text $ "td" @: [hasClass "title"] // "a" @: [hasClass "storylink"]
    return x

type Title = String
type Points = Int
data HomeRow
    = TitleRow Title (Maybe URL)
    | AuthorRow Points Author String Int
    deriving (Show, Eq)

mainBody :: Selector
mainBody = "table" @: [hasClass "itemlist"]

posts :: Scraper String [String]
posts = chroot mainBody posts'
    where
        posts' :: Scraper String [String]
        posts' = do
            rows <- innerHTMLs $ "tr" @: [notP (hasClass "spacer")]
            let rows = dropLast 2 rows
            return rows

homePage :: IO (Maybe [String])
homePage = scrapeURL "https://news.ycombinator.com" posts

getTopStories = homePage
