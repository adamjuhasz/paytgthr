module Chewpaca.DB.Tokens where

import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( query
                                                , Only(Only)
                                                , Connection
                                                )
import           Shared.Models.Token            ( UserToken(..)
                                                , userTokenFields
                                                )

getTokensForUser :: UUID -> Connection -> IO [UserToken]
getTokensForUser uid conn = query conn qs selector
 where
  qs =
    "SELECT "
      <> fst userTokenFields
      <> " FROM tgthr.tokens WHERE user_id = ? ORDER BY created_on DESC "
  selector = Only uid
