{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
module Lib where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Traversable as DT
import Data.Maybe

data ItemType
    = APITopStories
    | APIStory Int
    | APIComment Int
    deriving (Eq, Show)

baseURL = "https://hacker-news.firebaseio.com/v0/"

getURL :: ItemType -> String
getURL APITopStories  = baseURL ++ "topstories.json"
getURL (APIStory x)   = baseURL ++ "item/" ++ (show x) ++ ".json"
getURL (APIComment x) = baseURL ++ "item/" ++ (show x) ++ ".json"

type Author = String
data Comment = Comment { commentAuthor :: Author
                       , commentChildren :: [Comment]
                       } deriving (Eq, Show)

data Post
    = Link { linkTitle :: String
           , linkURL :: Maybe String
           , linkText :: Maybe String
           , linkAuthor :: Author
           , linkTime :: Integer
           , linkComments :: [Comment]
           }
    | Text { textTitle :: String
           , textText :: String
           , textAuthor :: Author
           , textTime :: Integer
           , textComments :: [Comment]
           }
    deriving (Eq, Show)

data Story = Story { storyTitle :: String
                   , storyURL :: Maybe String
                   , storyText :: Maybe String
                   , storyAuthor :: String
                   , storyTime :: Integer
                   , storyComments :: [Int]
                   } deriving (Show, Eq)

instance FromJSON Story where
    parseJSON (Object v) = Story <$>
        v .: "title" <*>
        v .:? "url" <*>
        v .:? "text" <*>
        v .: "by" <*>
        v .: "time" <*>
        v .:? "kids" .!= []
    parseJSON _ = mzero

getTopStories :: IO [IO Story]
getTopStories = do
    topStories <- getTopStories'
    return $ getStories topStories

getStories :: [Int] -> [IO Story]
getStories = map getStory

getStory :: Int -> IO Story
getStory x = do
    response <- get $ getURL (APIStory x)
    return $ unsafeDecode (response ^. responseBody)

getTopStories' :: IO [Int]
getTopStories' = do
    response <- get $ getURL APITopStories
    return $ unsafeDecode (response ^. responseBody)

unsafeDecode :: FromJSON a => L8.ByteString -> a
unsafeDecode = fromJust . decode
