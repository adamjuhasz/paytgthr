module Utils where

import           Data.Aeson.Encode.Pretty       ( Config(..)
                                                , Indent(Spaces)
                                                , NumberFormat(Generic)
                                                )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , iso8601DateFormat
                                                , parseTimeM
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.UUID                     as U
import           GHC.Stack                      ( HasCallStack )
import           Shared.Messages                ( MessageBody(ReplyV1)
                                                , ReplyMessage(ReplySuccessV1)
                                                , ReplySuccess(CmdSuccess)
                                                , TgthrMessage(..)
                                                )
import           Shared.Models.User             ( UserID(UserID) )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )

fromRight :: (HasCallStack, Show a) => Either a b -> b
fromRight (Right x) = x
fromRight (Left  e) = error $ show e

prettyConfig :: Config
prettyConfig = Config { confIndent          = Spaces 0
                      , confCompare         = compare
                      , confNumFormat       = Generic
                      , confTrailingNewline = False
                      }

aTime :: UTCTime
aTime = fromJust
  $ parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) "2019-01-01"

uuid1s :: Maybe U.UUID
uuid1s = U.fromString "00000000-0000-0000-0000-000000000000"

uuid2s :: Maybe U.UUID
uuid2s = U.fromString "00000000-0000-0000-0000-000000000000"

uuid3s :: Maybe U.UUID
uuid3s = U.fromString "00000000-0000-0000-0000-000000000000"

fullMsg :: TgthrMessage
fullMsg = TgthrMessage { tgthrMsgid     = MessageID . fromJust $ uuid3s
                       , tgthrParent    = MessageID <$> uuid2s
                       , tgthrSource    = MessageID <$> uuid1s
                       , tgthrBody      = ReplyV1 $ ReplySuccessV1 CmdSuccess
                       , tgthrTimestamp = aTime
                       , tgthrUserid    = Just $ UserID U.nil
                       }
