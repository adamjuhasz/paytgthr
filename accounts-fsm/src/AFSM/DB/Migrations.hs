{-# LANGUAGE QuasiQuotes #-}

module AFSM.DB.Migrations where

import           Database.PostgreSQL.Simple     ( Connection
                                                , execute_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )

createTokenTable :: Connection -> IO ()
createTokenTable conn = do
  putStrLn "Creating Table for tokens"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.tokens 
      ( created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , id UUID NOT NULL
      , revision INT8 NOT NULL
      , version STRING NOT NULL
      , msg_source UUID NOT NULL
      , token JSONB NOT NULL
      , token_code STRING NOT NULL
      , user_id UUID NOT NULL
      , created_on TIMESTAMPTZ NOT NULL
      , expires_at TIMESTAMPTZ
      , been_used BOOLEAN NOT NULL DEFAULT FALSE
      , CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC)
      ) 
    |]

  putStrLn "Giving Chewy access to table for tokens"
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.tokens TO chewpaca;
    |]

  putStrLn "Create table users_current_rev"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.users_current_rev 
      ( id UUID NOT NULL,
        status STRING NOT NULL,
        revision INT8 NOT NULL,
        version STRING NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        email STRING NULL,
        password STRING NULL,
        name_first STRING NULL,
        name_last STRING NULL,
        address_street STRING NULL,
        address_street2 STRING NULL,
        address_city STRING NULL,
        address_state STRING NULL,
        address_zip STRING NULL,
        bank_routing STRING NULL,
        bank_account STRING NULL,
        ssn STRING NULL,
        phone_number STRING NULL,
        dob TIMESTAMPTZ NULL,
        disclosure_accept TIMESTAMPTZ NULL,
        consent_accept TIMESTAMPTZ NULL,
        apto_cardholderid STRING NULL,
        apto_kyc_status JSONB NULL,
        apto_cardid STRING NULL,
        closure_reason STRING NULL,
        apto_cardstatus STRING NULL,
        msg_source UUID NOT NULL,
        bank_nickname STRING NULL,
        bank_name STRING NULL,
        dwolla_customerid STRING NULL,
        dwolla_fundingid STRING NULL,
        bank_type STRING NULL,
        bank_verified BOOLEAN NULL,
        bank_verified_amounts JSONB NULL,
        created_on TIMESTAMPTZ NOT NULL,
        first_signin_on TIMESTAMPTZ NULL,
        activated_on TIMESTAMPTZ NULL,
        card_created_on TIMESTAMPTZ NULL,
        card_activated_on TIMESTAMPTZ NULL,
        CONSTRAINT "primary" PRIMARY KEY (id ASC),
        UNIQUE INDEX one_email_one_user (id ASC, email ASC)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT, INSERT, UPDATE ON tgthr.users_current_rev TO chewpaca;
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.users_current_rev TO paymentauth;
    |]

  putStrLn "Create table groups_current_rev"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.groups_current_rev 
      ( id UUID NOT NULL,
        status STRING NOT NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        revision INT8 NOT NULL,
        version STRING NOT NULL,
        time_start TIMESTAMPTZ NULL,
        time_end TIMESTAMPTZ NULL,
        split JSONB NOT NULL,
        members JSONB NOT NULL,
        msg_source UUID NOT NULL,
        CONSTRAINT "primary" PRIMARY KEY (id ASC)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT, INSERT, UPDATE ON tgthr.groups_current_rev TO chewpaca;
    |]

  putStrLn "Create table cards if not exists"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.cards 
      ( id UUID NOT NULL,
        platform STRING NOT NULL,
        platform_id STRING NOT NULL,
        revision INT8 NOT NULL,
        design STRING NOT NULL,
        user_id UUID NOT NULL,
        status STRING NOT NULL,
        memo STRING NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        activated_at TIMESTAMPTZ NULL,
        closed_at TIMESTAMPTZ NULL,
        updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.cards TO chewpaca;
    |]

  putStrLn "Create table cards_current_rev if not exists"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.cards_current_rev
      ( id UUID NOT NULL,
        platform STRING NOT NULL,
        platform_id STRING NOT NULL,
        revision INT8 NOT NULL,
        design STRING NOT NULL,
        user_id UUID NOT NULL,
        status STRING NOT NULL,
        memo STRING NULL,
        created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        activated_at TIMESTAMPTZ NULL,
        closed_at TIMESTAMPTZ NULL,
        updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        CONSTRAINT "primary" PRIMARY KEY (id ASC)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.cards_current_rev TO chewpaca;
    |]

  putStrLn "Create table kyc_assesments if not exists"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.kyc_assesments
      ( user_id UUID NOT NULL,
        profile_id STRING NOT NULL,
        search_id STRING NOT NULL,
        passed BOOL NOT NULL,
        score_overall INT NOT NULL,
        failure_reasons JSONB NOT NULL,
        created_at TIMESTAMPTZ NOT NULL,

        score_phone INT NULL,
        score_address INT NULL,
        score_ssn INT NULL,
        score_name INT NULL,

        count_phone INT NOT NULL,
        count_address INT NOT NULL,
        count_ssn INT NOT NULL,
        count_name INT NOT NULL,

        input_phone STRING NULL,
        input_address STRING NULL,
        input_ssn STRING NULL,
        input_name STRING NULL,

        source_phone JSONB NOT NULL,
        source_address JSONB NOT NULL,
        source_ssn JSONB NOT NULL,
        source_name JSONB NOT NULL,
        
        updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
        CONSTRAINT "primary" PRIMARY KEY (search_id), 
        INDEX index_user_id (user_id)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.kyc_assesments TO chewpaca;
    |]

  putStrLn "Create table group_category_splits(_current_rev) if not exists"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.group_category_splits
      ( group_id UUID NOT NULL
      , category_id STRING NOT NULL
      , revision INT NOT NULL
      , created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , split JSONB NOT NULL
      , state STRING NOT NULL
      , user_split DECIMAL AS (((split->0->'ratio'->'numerator')::INT) / ((split->0->'ratio'->'denominator')::INT)) VIRTUAL
      , CONSTRAINT "primary" PRIMARY KEY (group_id, category_id, revision)
      );

    CREATE TABLE IF NOT EXISTS tgthr.group_category_splits_current_rev
      ( group_id UUID NOT NULL
      , category_id STRING NOT NULL
      , revision INT NOT NULL
      , created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , split JSONB NOT NULL
      , state STRING NOT NULL
      , user_split DECIMAL AS (((split->0->'ratio'->'numerator')::INT) / ((split->0->'ratio'->'denominator')::INT)) VIRTUAL
      , CONSTRAINT "primary" PRIMARY KEY (group_id, category_id)
      );
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.group_category_splits TO chewpaca;
    GRANT SELECT ON tgthr.group_category_splits_current_rev TO chewpaca;
    |]

  putStrLn "Create table user_notes(_current_rev) if not exists"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.user_notes
      ( user_id UUID NOT NULL
      , revision INT NOT NULL
      , created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , note STRING NOT NULL
      , CONSTRAINT "primary" PRIMARY KEY (user_id, revision)
      );

    CREATE TABLE IF NOT EXISTS tgthr.user_notes_current_rev
      ( user_id UUID NOT NULL
      , revision INT NOT NULL
      , created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , note STRING NOT NULL
      , CONSTRAINT "primary" PRIMARY KEY (user_id)
      );
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT ON tgthr.user_notes TO chewpaca;
    GRANT SELECT ON tgthr.user_notes_current_rev TO chewpaca;
    |]

  putStrLn "Create table user_device_ip if not exists"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.user_device_ip
      ( user_id UUID NOT NULL
      , ip_address INET NOT NULL
      , ip_country STRING NOT NULL
      , first_seen TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , last_seen TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
      , CONSTRAINT "primary" PRIMARY KEY (user_id, ip_address)
      );

    GRANT SELECT ON tgthr.user_device_ip TO chewpaca;
    |]

  putStrLn "Create Invite Table if not exist"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.partner_invites
        ( code STRING NOT NULL
        , inviter_id UUID NOT NULL
        , group_id UUID NULL
        , prefilled JSONB NOT NULL
        , status STRING NOT NULL
        , created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
        , updated_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
        , revision INT8 NOT NULL
        , CONSTRAINT "primary" PRIMARY KEY (code)
        );
      GRANT SELECT ON tgthr.partner_invites TO chewpaca;
    |]


  putStrLn "Create table rewards"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.rewards 
      ( id UUID NOT NULL,
        matcher JSONB NOT NULL,
        reward INT4 NOT NULL,
        expires_in_hr INT4 NOT NULL,
        name STRING NOT NULL,
        max_payout JSONB NOT NULL,
        active BOOL NOT NULL,
        created_at TIMESTAMPTZ NOT NULL,
        updated_at TIMESTAMPTZ NOT NULL,
        CONSTRAINT "primary" PRIMARY KEY (id ASC)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT, INSERT, UPDATE ON tgthr.rewards TO chewpaca;
    |]

  putStrLn "Create table reward_activations"
  _ <- execute_
    conn
    [sql| 
    CREATE TABLE IF NOT EXISTS tgthr.reward_activations 
      ( id UUID NOT NULL,
        reward_id UUID NOT NULL,
        group_id UUID NOT NULL,
        created_at TIMESTAMPTZ NOT NULL,
        updated_at TIMESTAMPTZ NOT NULL,
        changed_by_id UUID NOT NULL,
        expires_at TIMESTAMPTZ NOT NULL,
        state STRING NOT NULL,
        CONSTRAINT "primary" PRIMARY KEY (id ASC)
      ) 
    |]
  _ <- execute_
    conn
    [sql| 
    GRANT SELECT, INSERT, UPDATE ON tgthr.reward_activations TO chewpaca;
    |]

  putStrLn "Add uses to tables rewards & reward_activations"
  _ <- execute_
    conn
    [sql| 
    ALTER TABLE tgthr.rewards 
    ADD COLUMN IF NOT EXISTS uses INT4 NULL
    |]

  _ <- execute_
    conn
    [sql| 
    ALTER TABLE tgthr.reward_activations 
    ADD COLUMN IF NOT EXISTS uses_left INT4 NULL
    |]

  putStrLn "Add table referral_programs"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.referral_programs 
        ( id UUID NOT NULL PRIMARY KEY,
          workflow JSONB NOT NULL,
          reward_referrer JSONB NOT NULL,
          reward_referee JSONB NOT NULL,
          active BOOL NOT NULL,
          openaccess BOOL NOT NULL,
          created_at TIMESTAMPTZ NOT NULL,
          updated_at TIMESTAMPTZ NOT NULL,
          revision INT NOT NULL,
          name STRING NOT NULL
        ) 
    |]

  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.referral_programs to chewpaca;
    |]

  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.referral_programs_historical (
        LIKE tgthr.referral_programs INCLUDING DEFAULTS,
        CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC)
      )
    |]

  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.referral_programs_historical to chewpaca;
    |]

  putStrLn "Add table referral_codes"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.referral_codes 
        ( code STRING NOT NULL PRIMARY KEY,
          referrer_id UUID NOT NULL,
          program_id UUID NOT NULL,
          created_at TIMESTAMPTZ NOT NULL
        ) 
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.referral_codes to chewpaca;
    |]

  putStrLn "Add table referral_progress"
  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.referral_progress 
        ( id UUID NOT NULL PRIMARY KEY,
          program_id UUID NOT NULL,
          referee_id UUID NOT NULL,
          referrer_id UUID NOT NULL,
          expiration_at TIMESTAMPTZ NULL,
          progress JSONB NOT NULL,
          progress_display INT NOT NULL,
          created_at TIMESTAMPTZ NOT NULL,
          updated_at TIMESTAMPTZ NOT NULL,
          revision INT NOT NULL
        ) 
    |]
  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.referral_progress to chewpaca;
    |]

  _ <- execute_
    conn
    [sql| 
      CREATE TABLE IF NOT EXISTS tgthr.referral_progress_historical (
        LIKE tgthr.referral_progress INCLUDING DEFAULTS,
        CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC)
      )
    |]

  _ <- execute_
    conn
    [sql| 
      GRANT SELECT ON tgthr.referral_progress_historical to chewpaca;
    |]

  return ()
