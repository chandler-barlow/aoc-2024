{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (try, SomeException)

import qualified Day.One
import qualified Day.Two

withInput :: FilePath -> (String -> IO a) -> IO (Either String a)
withInput path fn = do
  res <- try @SomeException $ readFile path
  case res of 
    Right content -> Right <$> fn content
    Left _ -> pure $ Left "Could not open specific file"

main :: IO ()
main = do 
  res <- withInput Day.Two.testPath (pure . Day.Two.partOne)
  print res
