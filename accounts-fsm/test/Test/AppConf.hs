module Test.AppConf where

import           AFSM.AppMonad                  ( AppConfig(AppConfig) )
import           AFSM.DB.DBActions              ( DBActions(..) )

appConf :: DBActions -> AppConfig
appConf db = AppConfig db
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
                       undefined
