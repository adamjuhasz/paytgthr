{-# LANGUAGE QuasiQuotes #-}

module Chewpaca.DB.Cards where

import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(..)
                                                , query
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Card             ( CardModel
                                                , cardSqlFields
                                                )
import           Shared.Models.Ids              ( CardId
                                                , UserID
                                                )

getCardsFor :: UserID -> Connection -> IO [CardModel]
getCardsFor userId conn = query
  conn
  ([sql| SELECT |]
  <> fst cardSqlFields
  <> [sql| FROM tgthr.cards_current_rev WHERE user_id = ? ORDER BY created_at DESC, id ASC |]
  )
  (Only userId)

getCardId :: CardId -> Connection -> IO [CardModel]
getCardId cid conn = query
  conn
  ([sql| SELECT |] <> fst cardSqlFields <> [sql| 
  FROM tgthr.cards 
  WHERE id = ?
  ORDER BY created_at DESC, id ASC |]
  )
  (Only cid)
