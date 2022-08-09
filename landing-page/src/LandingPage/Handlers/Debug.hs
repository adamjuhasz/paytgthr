
module LandingPage.Handlers.Debug where

import           Data.Functor                   ( (<&>) )
import qualified Data.Text.Lazy                as TL
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( expectSession
                                                , requireUser
                                                )
import           Network.Wai                    ( Request(vault) )
import           Shared.Amqp                    ( AMQPPublisher )
import           Shared.Amqp.Utils              ( getGroupForUser
                                                , getUser
                                                )
import           Shared.Models.Group            ( GroupModel(grpStatus) )
import           Shared.Models.User             ( UserModel(usrUserState) )
import           Web.Scotty                     ( ActionM
                                                , liftAndCatchIO
                                                , request
                                                , text
                                                )

accountStatus :: V.Key SessionData -> AMQPPublisher -> ActionM ()
accountStatus sKey pub = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  model <- liftAndCatchIO $ getUser pub user
  case model of
    Left  _ -> text "Nothing"
    Right m -> text . TL.pack . show $ usrUserState m

dumpSessionHandler :: V.Key SessionData -> ActionM ()
dumpSessionHandler sKey = do
  req <- request
  let session = V.lookup sKey (vault req)
  liftAndCatchIO $ print session
  case session of
    Nothing -> text "Unknown session?!"
    Just s  -> text . TL.pack . show $ s

groupStatus :: V.Key SessionData -> AMQPPublisher -> ActionM ()
groupStatus sKey pub = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  model <- liftAndCatchIO $ getGroupForUser pub user
  case model of
    Left  _ -> text "Nothing"
    Right m -> text . TL.pack . show $ grpStatus m


