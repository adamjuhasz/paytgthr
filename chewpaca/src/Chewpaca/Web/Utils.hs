{-# LANGUAGE FlexibleContexts #-}

module Chewpaca.Web.Utils where

import           Data.Coerce                    ( Coercible
                                                , coerce
                                                )
import           Data.Text                      ( Text
                                                , splitOn
                                                )
import           Data.UUID                      ( UUID
                                                , toText
                                                )

uuidToStrStart :: Coercible a UUID => a -> Text
uuidToStrStart u = head $ splitOn "-" $ toText $ coerce u
