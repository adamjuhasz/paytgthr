module Chewpaca.DB.KYC where

import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(..)
                                                , query
                                                )
import           Shared.Models.KYCAssesment     ( KYCAssesment
                                                , kycAssesmentFields
                                                )
import           Shared.Models.User             ( UserID )

getUsersKYCAssesments :: UserID -> Connection -> IO [KYCAssesment]
getUsersKYCAssesments uid conn = query conn qs (Only uid)
 where
  qs =
    "SELECT "
      <> fst kycAssesmentFields
      <> " FROM tgthr.kyc_assesments WHERE user_id = ?"
