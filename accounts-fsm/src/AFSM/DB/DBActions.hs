{-# LANGUAGE StrictData #-}

module AFSM.DB.DBActions where

import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Shared.Models.Cardholder       ( CardholderId )
import           Shared.Models.Group            ( GroupId
                                                , GroupModel
                                                , GroupStatus(..)
                                                )
import           Shared.Models.Token            ( TokenText
                                                , UserToken(..)
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , PhoneNumber(..)
                                                , RedactedText(..)
                                                , UserID(..)
                                                , UserModel(..)
                                                )

data DBActions = DBActions
  { cSaveUser                  :: UserModel -> IO ()
  , cGetAccountByEmail         :: EmailAddress -> IO (Maybe UserModel)
  , cGetAccountByPhone         :: PhoneNumber -> IO (Maybe UserModel)
  , cGetAccountById            :: UserID -> IO (Maybe UserModel)
  , cGetAccounByCardholder     :: CardholderId -> IO (Maybe UserModel)
  , cSaveGroup                 :: GroupModel -> IO ()
  , cGetGroupById              :: GroupId -> IO (Maybe GroupModel)
  , cGetGroupsByUserId         :: UserID -> IO [GroupModel]
  , cGetGroupsByUserIdFiltered :: [GroupStatus] -> UserID -> IO [GroupModel]
  , cGetAllActiveUsers         :: IO [UserID]
  , cGetUsersWithAcountNumber  :: RedactedText -> RedactedText -> IO [UserID]
  , cGetUsersWithSSN           :: Text -> IO [UserID]
  , cFindToken                 :: TokenText -> UserID -> IO [UserToken]
  , cSaveToken                 :: UserToken -> IO Int64
  }
