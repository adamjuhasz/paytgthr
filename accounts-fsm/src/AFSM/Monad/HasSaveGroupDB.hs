module AFSM.Monad.HasSaveGroupDB where


import           Shared.Models.CategorySplit    ( CategorySplit(..) )
import           Shared.Models.Group            ( GroupModel(..) )

class Monad m => HasSaveGroupDB m where
  saveGroupModel :: GroupModel -> m ()
  saveCategorySplit :: CategorySplit -> m ()
