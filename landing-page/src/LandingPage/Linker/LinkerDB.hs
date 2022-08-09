{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, StrictData, RecordWildCards #-}

module LandingPage.Linker.LinkerDB where

import           GHC.Generics
import           Data.Aeson
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Web.Hashids                    ( HashidsContext )
import qualified Web.Hashids                   as Hsh

type LinkRetriever = ByteString -> IO (Maybe ShortenedLink)
type LinkStorer = ShortenedLink -> IO ByteString

data ShortLinkTypes
  = RedirectLink { redirectUrl :: Text }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField ShortLinkTypes where
  toField = toJSONField
instance FromField ShortLinkTypes where
  fromField = fromJSONField

data ShortenedLink = ShortenedLink
  { linkId :: Int
  , linkType :: ShortLinkTypes
  , linkExpiration :: UTCTime
  }
  deriving (Eq, Show)
instance ToRow ShortenedLink where
  toRow ShortenedLink {..} =
    [toField linkId, toField linkType, toField linkExpiration]
instance FromRow ShortenedLink where
  fromRow = do
    linkId         <- field
    linkType       <- field
    linkExpiration <- field
    return ShortenedLink { .. }

retriveLink
  :: Int
  -> HashidsContext
  -> ByteString
  -> Connection
  -> IO (Maybe ShortenedLink)
retriveLink minHashLen ctx anId conn = if B.length anId < minHashLen
  then putStrLn "< minHashLen" >> return Nothing
  else case Hsh.decode ctx anId of
    [theId] -> listToMaybe <$> query conn qs (Only theId)
    _       -> return Nothing
  where qs = [sql| SELECT id, type, expiration FROM tgthr.links where id = ? |]

storeLink :: HashidsContext -> ShortenedLink -> Connection -> IO ByteString
storeLink ctx ShortenedLink {..} conn = do
  lids :: [Only Int] <- query conn qs selector
  case lids of
    [Only lid] -> return $ Hsh.encode ctx lid
    _          -> error "bad insert"
 where
  qs
    = [sql| INSERT INTO tgthr.links (id, type, expiration) VALUES (DEFAULT, ?, ?) RETURNING id |]
  selector = (linkType, linkExpiration)
