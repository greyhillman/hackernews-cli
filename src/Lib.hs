{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    , getTopStories
    ) where

--import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import Network.Wreq
import Control.Lens
import Control.Monad


type Author = String
data Comment = Comment {
                   commentAuthor :: Author,
                   commentChildren :: [Comment]
                } deriving (Eq, Show)

data Post = Post {
                postTitle :: String,
                postURL :: Maybe String,
                postText :: Maybe String,
                postAuthor :: Author,
                postTime :: Integer,
                postComments :: [Int]
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

getTopStories :: IO [Post]
getTopStories = do
    post_ids <- getTopStories'
    posts <- getPosts post_ids
    return (take 30 posts)

getPosts :: [Int] -> IO [Post]
getPosts = mapM getPost

getPost :: Int -> IO Post
getPost id = do
    response <- get $ "https://hacker-news.firebaseio.com/v0/item/" ++ (show id) ++ ".json"
    let body = response ^. responseBody
    let post = case (decode body) of
            Just x -> x
            _ -> error "The API did not return what it said it would :("
    return post

-- Only get the top 30 stories
getTopStories' :: IO [Int]
getTopStories' = do
    response <- get "https://hacker-news.firebaseio.com/v0/topstories.json"
    let body = response ^. responseBody
    return $ case decode body of
        Just x -> take 30 x
        _ -> error "The API is not returning what it said it would"

getStory :: Int -> IO Post
getStory = undefined

getComment :: Int -> IO Comment
getComment = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
