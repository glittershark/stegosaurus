module Main where

import Prelude

import Web.Stegosaurus.DBPedia
import Data.Hourglass.Types (Date)

main :: IO ()
main = (birthDateOf "Benjamin_Franklin" :: IO (Either SomeException Date))
       >>= print
