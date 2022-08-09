module Shared.Transactions.Categorize where

import           Shared.Models.CategorySplit    ( CategoryCode(..) )
import           Shared.Models.Transaction      ( Transaction )
import           Shared.Transactions.Categories.Cat001
                                                ( category001 )
import           Shared.Transactions.Categories.Cat002
                                                ( category002 )
import           Shared.Transactions.Categories.Cat003
                                                ( category003 )
import           Shared.Transactions.Categories.Cat004
                                                ( category004 )
import           Shared.Transactions.Categories.Cat005
                                                ( category005 )
import           Shared.Transactions.Categories.Cat006
                                                ( category006 )

categorize :: Transaction -> [(CategoryCode, Bool)]
categorize trx =
  [ (Category000, True)
  , (Category001, category001 trx)
  , (Category002, category002 trx)
  , (Category003, category003 trx)
  , (Category004, category004 trx)
  , (Category005, category005 trx)
  , (Category006, category006 trx)
  ]
