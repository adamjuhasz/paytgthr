{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Group List handler
Description : Get a list of all groups for a user
Maintainer  : adam@example.com
Stability   : experimental
-}
module LandingPage.Handlers.Application.GroupCreate where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(Null)
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( nubBy )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Text                      ( Text )
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           LandingPage.Validators         ( emailCheck
                                                , nameValidator
                                                )
import           Network.HTTP.Types             ( status400
                                                , status403
                                                , status409
                                                , status428
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupModel(..)
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , UserID(..)
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( CreateGroupBody(..)
                                                , Routes(..)
                                                , asClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , request
                                                , status
                                                )

createGroup :: V.Key SessionData -> ClientEnv -> ActionM ()
createGroup sKey accountsEnv = do
  uid <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  requestedMembers <- jsonData :: ActionM [(Text, EmailAddress)]
  newGid           <- liftAndCatchIO (GroupId <$> nextRandom)
  trace            <- createTrace

  -- verify emails are valid
  let allValid = and $ fmap
        (\(fname, EmailAddress email) ->
          isJust (nameValidator fname) && isJust (emailCheck email)
        )
        requestedMembers

  unless
    allValid
    (  status status400
    >> Scotty.json
         (object
           [ "group" .= Null
           , "errors" .= object
             [ ("nameError" , "Empty")
             , ("emailError", "Empty")
             , "alreadyInGroup" .= False
             ]
           ]
         )
    >> finish
    )

  -- grab this users info and add to the list
  let getUser = _UserGet asClientM trace uid
  userEi <- liftAndCatchIO $ runClientM getUser accountsEnv
  user   <- case userEi of
    Left e -> do
      liftAndCatchIO $ putStr "Error: createGroup _UserGet " >> print
        (uid, newGid, e)
      status status403
        >> Scotty.json
             (object
               [ "group" .= Null
               , "errors" .= object
                 [ ("nameError" , "None")
                 , ("emailError", "None")
                 , "alreadyInGroup" .= False
                 ]
               ]
             )
        >> finish
    Right u -> return u

  let fullMembers =
        (fromMaybe "Partner" (usrFirstName user), usrEmail user)
          : requestedMembers
  let uniqeMembers =
        nubBy (\(_, email1) (_, email2) -> email1 == email2) fullMembers

  when
    (length uniqeMembers /= length fullMembers)
    (do
      liftAndCatchIO $ putStr "Warning createGroup: Self Invite" >> print
        (uid, newGid)
      status status409
        >> Scotty.json
             (object
               [ "group" .= Null
               , "errors" .= object
                 [ ("nameError" , "None")
                 , ("emailError", "SelfInvite")
                 , "alreadyInGroup" .= False
                 ]
               ]
             )
        >> finish
    )

  membersWithUID <- mapM
    (\(name, email) -> (name, email, ) . UserID <$> liftAndCatchIO nextRandom)
    fullMembers
  let createGroupBody = CreateGroupBody { members      = membersWithUID
                                        , inviter      = uid
                                        , validThrough = Nothing
                                        }
  let createGroupFn = _GroupCreate asClientM trace newGid createGroupBody
  res <- liftAndCatchIO $ runClientM createGroupFn accountsEnv
  case res of
    Left e -> do
      liftAndCatchIO $ putStr "Error createGroup: " >> print (uid, newGid, e)
      status status500
        >> Scotty.json
             (object
               [ "group" .= Null
               , "errors" .= object
                 [ ("nameError" , "None")
                 , ("emailError", "None")
                 , "alreadyInGroup" .= False
                 ]
               ]
             )
        >> finish
    Right GroupModel {..} -> if newGid /= grpId
      then status status428 >> Scotty.json
        (object
          [ "group" .= grpId
          , "errors"
            .= object
                 [ ("nameError" , "None")
                 , ("emailError", "None")
                 , "alreadyInGroup" .= True
                 ]
          ]
        )
      else Scotty.json $ object
        [ "group" .= grpId
        , "errors" .= object
          [ ("nameError" , "None")
          , ("emailError", "None")
          , "alreadyInGroup" .= False
          ]
        ]

