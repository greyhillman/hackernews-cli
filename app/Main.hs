{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Lib
import Control.Monad
import Control.Concurrent.ParallelIO


printSimplePost :: Int -> Story -> IO ()
printSimplePost x (Story { storyTitle }) = do
    putStrLn $ (show x) ++ " " ++ storyTitle

main :: IO ()
main = do
    topStories <- getTopStories
    topStories' <- parallel $ take 30 topStories
    zipWithM_ printSimplePost [1..] topStories'
    stopGlobalPool
    
