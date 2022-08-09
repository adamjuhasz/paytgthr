{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIDwolla.InternalAPI.FsCreate where

import qualified APIDwolla.FundingSource.Create
                                               as FSC
import           APIDwolla.InternalAPI.ProcessChanges
                                                ( processChanges )
import           APIDwolla.Monad.Accounts       ( HasAccounts )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient )
import           APIDwolla.Monad.Payment        ( HasPayments(..) )
import           Control.Monad.Catch            ( MonadCatch(catch) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8    as C
import           Servant                        ( ServerError(errBody)
                                                , err403
                                                )
import           Shared.Console
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.ApiDwolla.API    ( CreateFSBody(..)
                                                , CreateFSResponse(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

createfundingSource
  :: ( HasAccounts m
     , HasDwollaClient m
     , MonadIO m
     , HasPayments m
     , MonadError ServerError m
     , MonadCatch m
     )
  => TraceContext
  -> UserID
  -> CreateFSBody
  -> m CreateFSResponse
createfundingSource trace userId CreateFSBody {..} = do
  tracePrint trace
             "APIDwolla.InternalAPI.FsCreate.createfundingSource "
             (userId, abaRoutingNo, accountName)

  (fsId, evts) <-
    FSC.createBankAccount trace userId abaRoutingNo ddaAccountNo accountName
      `catch` (\(e :: FSC.CreateBankAccountErrors) ->
                throwError err403 { errBody = C.pack $ show (userId, e) }
              )

  mapM_ (processChanges trace userId) evts
  return $ CreateFSResponse fsId

