module Web.Stegosaurus.DBPedia.Types where

import Prelude

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Control.Stegosaurus
import Web.Stegosaurus.Exceptions

newtype URI = URI { uriValue :: Text } deriving (Show, Read, Eq)

data ODataValue = LiteralValue Text | DeferredValue URI deriving (Show, Read, Eq)

instance FromJSON ODataValue where
  parseJSON (Object o) =
    if "__deferred" `HM.member` o
      then liftM (DeferredValue . URI) $ o .: "__deferred" >>= (.: "uri")
      else DeferredValue . URI <$> o .: "uri"
  parseJSON (String s) = return $ LiteralValue s
  parseJSON v = typeMismatch "Object|String" v

data ODataResponse = ODataResponse { properties :: HM.HashMap Text ODataValue
                                   } deriving (Show, Read, Eq)

instance FromJSON ODataResponse where
  parseJSON (Object topLevel) =
    topLevel .: "d" >>= (.: "results") >>= withArray "results"
      (\a -> ODataResponse <$> parseJSON (V.head a))
  parseJSON v = typeMismatch "Object" v

prop :: MonadThrow m => Text -> ODataResponse -> m ODataValue
prop key resp = throwIfNothing (PropNotFound key)
              . HM.lookup key
              . properties
              $ resp
