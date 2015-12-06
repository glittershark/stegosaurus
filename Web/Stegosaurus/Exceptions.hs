module Web.Stegosaurus.Exceptions where

import Prelude

data PropNotFound = PropNotFound Text deriving (Typeable)

instance Show PropNotFound where
    show (PropNotFound p') = let p = unpack p' in
      "Property " ++ p ++ " was not found in the response"

instance Exception PropNotFound

data CouldNotDecode = CouldNotDecode deriving (Typeable)

instance Show CouldNotDecode where
    show _ = "Could not decode JSON"

instance Exception CouldNotDecode

data BadDate = BadDate String deriving (Typeable)

instance Show BadDate where
    show (BadDate d) = "Could not parse date: " ++ d

instance Exception BadDate
