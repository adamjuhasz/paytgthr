{-# LANGUAGE QuasiQuotes #-}

module LandingPage.DB.Migrations where

import           Database.PostgreSQL.Simple     ( Connection
                                                , execute_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )

runMigration :: Connection -> IO ()
runMigration conn = do
  putStrLn "Creating Table for tgthr.app_events if not exists"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.app_events 
      ( id UUID NOT NULL DEFAULT gen_random_uuid()
      , created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , event_name STRING NOT NULL
      , event_properties JSONB NULL
      , user_id UUID NULL
      , device_id STRING NOT NULL
      , device_info JSONB NOT NULL
      , cellular_info JSONB NOT NULL
      , device_permissions JSONB NOT NULL
      , app_info JSONB NOT NULL
      , CONSTRAINT "primary" PRIMARY KEY (id ASC)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.app_events TO chewpaca;
    |]

  return ()
