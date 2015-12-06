module Web.Stegosaurus.DBPedia ( birthDateOf
                               ) where

import Prelude hiding (decodeUtf8)

import Data.Hourglass.Types (Date, dtDate)
import Data.Hourglass (timeParse, ISO8601_Date(..))
import Network.Wreq (get, asJSON, responseBody)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict as HM
import Control.Lens ((^.))

import Control.Stegosaurus
import Web.Stegosaurus.Exceptions
import Web.Stegosaurus.DBPedia.Types

dbpediaURL :: Text -> String
dbpediaURL page' = let page = unpack page' in
  -- http://dbpedia.org/data/Benjamin_Franklin.jsod
  "http://dbpedia.org/data/" ++ page ++ ".jsod"

birthDateProp :: IsString a => a
birthDateProp = "http://dbpedia.org/ontology/birthDate"

parseDate :: MonadThrow m => ODataValue -> m Date
parseDate (LiteralValue v') = let v = unpack v' in
  throwIfNothing (BadDate v) $ dtDate <$> timeParse ISO8601_Date v
parseDate _ = throwM $ BadDate "deferred value"

birthDateOf :: MonadThrow m => Text -> IO (m Date)
birthDateOf person = do
    resp <- get $ dbpediaURL person
    return . throwIfNothing CouldNotDecode
           $ Aeson.decode (resp^.responseBody)
             >>= prop birthDateProp
             >>= parseDate

