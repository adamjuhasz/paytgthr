{-# LANGUAGE StrictData, RecordWildCards, NamedFieldPuns #-}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Reduce duplication" -}

{-|
Module      : Callback
Description : Large callback function for MQ
Maintainer  : adam@example.com
Stability   : experimental
-}
module AFSM.Callback where

import           AFSM.AppMonad                  ( AppConfig
                                                , AppIOM(unAppIOM)
                                                )
import           AFSM.Group.Query.ByGroupState  ( getAliveGroups )
import           AFSM.User.Query.User           ( getUserWithId )
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT) )
import           Shared.Amqp                    ( AMQPCallback
                                                , CallbackResults
                                                , EventMessage(..)
                                                , MessageBody(..)
                                                , ReplyFailure(AcctFailure)
                                                , ReplyMessage(..)
                                                , ReplySuccess(AcctReplySuccess)
                                                , TgthrMessage(..)
                                                , genSuccess
                                                )
import           Shared.Messages                ( CommandMessage(..) )
import           Shared.TgthrMessages.Accounts  ( AccountReplies(..)
                                                , AccountReplyFailues(..)
                                                , AccountsCmd(..)
                                                , AccountsEvent(..)
                                                )

prepareMsgs :: ([AccountsEvent], a) -> IO CallbackResults
prepareMsgs (x, _) = return (Just genSuccess, fmap AccountsEvt x)

wrapSucc :: AccountReplies -> Maybe ReplyMessage
wrapSucc = Just . ReplySuccessV1 . AcctReplySuccess

wrapFail :: AccountReplyFailues -> Maybe ReplyMessage
wrapFail = Just . ReplyFailureV1 . AcctFailure

callback :: AppConfig -> AMQPCallback
callback appConfig _ _ TgthrMessage { tgthrBody = CommandV1 (AcctCmd GetUser { gucUser }) }
  = do
    putStrLn "RabbitMQ CommandV1 AcctCmd GetUser"
    let fn = getUserWithId gucUser
    user <- runReaderT (unAppIOM fn) appConfig
    return $ case user of
      Just u  -> (wrapSucc $ GetUserReply gucUser u, [])
      Nothing -> (wrapFail $ GetUserReplyFailure gucUser, [])

callback appConfig _ _ TgthrMessage { tgthrBody = CommandV1 (AcctCmd GetUsersActivePendingGroups {..}) }
  = do
    putStrLn "RabbitMQ CommandV1 AcctCmd GetUsersActivePendingGroups"
    let fn = getAliveGroups gucUser
    groups <- runReaderT (unAppIOM fn) appConfig
    return (wrapSucc $ GetUserGroupReply gucUser groups, [])

callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (PayCmd _) } =
  return (Nothing, [])
callback _ _ _ TgthrMessage { tgthrBody = CommandV1 (AptoCmd _) } =
  return (Nothing, [])
callback _ _ _ TgthrMessage { tgthrBody = EventV1 _ } = return (Nothing, [])
callback _ _ _ TgthrMessage { tgthrBody = ReplyV1 _ } = return (Nothing, [])
