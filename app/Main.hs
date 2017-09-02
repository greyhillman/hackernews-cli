{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Lib
import Control.Monad


printSimplePost :: Int -> String -> IO ()
printSimplePost x title = do
    putStrLn $ (show x) ++ " " ++ title

main :: IO ()
main = do
    posts <- getTopStories
    case posts of
        Just xs -> do
            zipWithM printSimplePost [1..] xs
            return ()
        Nothing -> putStrLn "Failed to get HackerNews"
