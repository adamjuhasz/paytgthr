module Scaffolding.Categories where

import qualified Data.Time.Clock               as Clock
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Scaffolding.Groups             ( basicGroup )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                )
import           Shared.Models.CategorySplit    ( CategoryCode(..)
                                                , CategorySplit(..)
                                                , CategoryState(..)
                                                )
import           Shared.Models.Group            ( GroupModel(grpId, grpSplit)
                                                , GroupSplit
                                                  ( GroupSplit
                                                  , splApproved
                                                  , splRatio
                                                  , splUser
                                                  )
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )

cat000 :: CategorySplit
cat000 = CategorySplit { groupId     = grpId basicGroup
                       , categoryId  = Category000
                       , catRevision = 1
                       , createdAt   = unsafePerformIO Clock.getCurrentTime
                       , updatedAt   = unsafePerformIO Clock.getCurrentTime
                       , splits      = grpSplit basicGroup
                       , state       = CategoryActive
                       }

cat001 :: CategorySplit
cat001 = CategorySplit
  { groupId     = grpId basicGroup
  , categoryId  = Category001
  , catRevision = 1
  , createdAt   = unsafePerformIO Clock.getCurrentTime
  , updatedAt   = unsafePerformIO Clock.getCurrentTime
  , splits      = [ GroupSplit { splUser     = usrUserID userJohn
                               , splRatio    = 90
                               , splApproved = True
                               }
                  , GroupSplit { splUser     = usrUserID userJane
                               , splRatio    = 10
                               , splApproved = True
                               }
                  ]
  , state       = CategoryActive
  }
