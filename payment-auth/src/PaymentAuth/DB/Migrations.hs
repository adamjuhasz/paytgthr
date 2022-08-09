{-# LANGUAGE QuasiQuotes #-}

module PaymentAuth.DB.Migrations where

import           Database.PostgreSQL.Simple     ( Connection
                                                , execute_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )

addPaymentACHInfo :: Connection -> IO ()
addPaymentACHInfo conn = do
  putStrLn
    "Adding fields to payments table (pay_subtype, pay_achinfo, pay_failurecode)"
  _ <- execute_
    conn
    [sql| ALTER TABLE tgthr.payments ADD COLUMN IF NOT EXISTS pay_subtype JSONB NOT NULL DEFAULT '"InitialVerification"',  ADD COLUMN IF NOT EXISTS pay_achinfo JSONB NULL, ADD COLUMN IF NOT EXISTS pay_failurecode JSONB NULL |]
  _ <- execute_ conn [sql| DROP VIEW IF EXISTS tgthr.payments_collapsed |]
  _ <- execute_
    conn
    [sql| CREATE VIEW 
	    tgthr.payments_collapsed
	    AS SELECT DISTINCT ON (id) id, revision, version, msg_source, status, transaction_id, user_id, pay_type, pay_method, pay_amount, description, pay_method_id, created_at, "visible", pay_subtype, pay_achinfo, pay_failurecode   
	    FROM tgthr.public.payments ORDER BY id DESC, revision DESC 
    |]
  _ <- execute_ conn
                [sql| GRANT SELECT ON tgthr.payments_collapsed to chewpaca; |]
  return ()

createNewCurrentRevTable :: Connection -> IO ()
createNewCurrentRevTable conn = do
  putStrLn "Creating Table ledger_current_rev"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.ledger_current_rev (
        LIKE tgthr.ledger INCLUDING DEFAULTS,
        CONSTRAINT "primary" PRIMARY KEY (user_id ASC),
        UNIQUE INDEX enforce_unique_id (id ASC),
        INDEX ledger_user_id_created_at_idx (user_id ASC, created_at DESC)
      );
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT, INSERT, UPDATE ON tgthr.ledger_current_rev to chewpaca;
    |]

  putStrLn "Creating Table payments_current_rev"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.payments_current_rev (
        LIKE tgthr.payments INCLUDING DEFAULTS,
        CONSTRAINT "primary" PRIMARY KEY (id ASC),
        INVERTED INDEX amount_idx (pay_amount)
      );
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT, INSERT, UPDATE ON tgthr.payments_current_rev to chewpaca;
    |]

  putStrLn "Creating Table riskscores_current_rev"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.riskscores_current_rev (
        LIKE tgthr.riskscores INCLUDING DEFAULTS,
        CONSTRAINT "primary" PRIMARY KEY (user_id ASC),
        INDEX ledger_user_id_created_at_idx (user_id ASC, created_at DESC)
      );
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT, INSERT, UPDATE ON tgthr.riskscores_current_rev to chewpaca;
    |]

  putStrLn "Creating Table transactions_current_rev"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.transactions_current_rev (
        LIKE tgthr.transactions INCLUDING DEFAULTS,
        splitting_users STRING[] AS (ARRAY[split_amounts->0->>0,split_amounts->1->>0]) STORED, 
        CONSTRAINT "primary" PRIMARY KEY (id ASC),
        INVERTED INDEX dispute_idx (dispute_info),
        INVERTED INDEX decline_idx (decline_info),
        INVERTED INDEX amount_display_idx (amount_display),
        INVERTED INDEX billing_idx (amount_billing),
        INVERTED INDEX details_idx (details),
        INVERTED INDEX split_idx (split_amounts),
        INVERTED INDEX merchant_idx (merchant)
      );
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT, INSERT, UPDATE ON tgthr.transactions_current_rev to chewpaca;
    |]

  putStrLn "Creating Table journals"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.journals (
        id UUID NOT NULL DEFAULT gen_random_uuid(),
        kind STRING NOT NULL,
        link UUID NULL,
        type_json JSONB NOT NULL,
        name STRING NOT NULL,
        user_id UUID NULL,
        last_entry UUID NOT NULL,
        balance JSONB NOT NULL,
        balance_number DECIMAL(10,2) NOT NULL,
        balance_pending JSONB NOT NULL,
        balance_pending_number DECIMAL(10,2) NOT NULL,
        revision INT8 NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC)
      );
      CREATE TABLE IF NOT EXISTS tgthr.journals_current_rev (
        LIKE tgthr.journals INCLUDING DEFAULTS,
        CONSTRAINT "primary" PRIMARY KEY (id ASC)
      );
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.journals to chewpaca;
      GRANT SELECT ON tgthr.journals_current_rev to chewpaca;
    |]

  putStrLn "Creating Table journals"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.journals (
        id UUID NOT NULL DEFAULT gen_random_uuid(),
        kind STRING NOT NULL,
        link UUID NULL,
        type_json JSONB NOT NULL,
        name STRING NOT NULL,
        user_id UUID NULL,
        last_entry UUID NOT NULL,
        balance JSONB NOT NULL,
        balance_number DECIMAL(10,2) NOT NULL,
        balance_pending JSONB NOT NULL,
        balance_pending_number DECIMAL(10,2) NOT NULL,
        revision INT8 NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        transaction UUID NOT NULL,
        CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC)
      );
      CREATE TABLE IF NOT EXISTS tgthr.journals_current_rev (
        LIKE tgthr.journals INCLUDING DEFAULTS,
        CONSTRAINT "primary" PRIMARY KEY (id ASC)
      );
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.journals to chewpaca;
      GRANT SELECT ON tgthr.journals_current_rev to chewpaca;
    |]

  putStrLn "Creating Table ledger transactions"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.ledger_transaction (
        id UUID NOT NULL DEFAULT gen_random_uuid(),
        entries_id JSONB NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        fact JSONB NOT NULL,
        idempotency STRING NULL,
        CONSTRAINT "primary" PRIMARY KEY (id ASC)
      );
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.ledger_transaction to chewpaca;
    |]

  -- putStrLn "Adding fields to transactions (reward_id)"
  -- _ <- execute_
  --   conn
  --   [sql| ALTER TABLE tgthr.transactions ADD COLUMN IF NOT EXISTS reward_id UUID |]
  -- _ <- execute_
  --   conn
  --   [sql| ALTER TABLE tgthr.transactions_current_rev ADD COLUMN IF NOT EXISTS reward_id UUID |]

  return ()
