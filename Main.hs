module Main where

import Prelude

import Web.Stegosaurus.DBPedia
import Data.Hourglass.Types (Date)

person :: IO Text
person = do
    args <- getArgs
    return $ fromMaybe deflt $ headMay args
    where deflt = "Benjamin_Franklin"

main :: IO ()
main = do
    bd <- birthDateOf =<< person :: IO (Either SomeException Date)
    case bd of
      Left ex -> putStrLn $ "Could not find birth date: " ++ tshow ex
      Right d -> print d
