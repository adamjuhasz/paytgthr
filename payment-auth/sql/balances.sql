CREATE TABLE IF NOT EXISTS tgthr.balances (
	id UUID NOT NULL DEFAULT gen_random_uuid(),
	created_at TIMESTAMPTZ NULL DEFAULT now():::TIMESTAMPTZ,
	balance_current DECIMAL(10,3) NOT NULL,
	balance_available DECIMAL(10,3) NULL,
	account_type STRING NOT NULL,
	account_name STRING NULL,
	account_id STRING NOT NULL,
	user_id UUID NOT NULL,
	access_time_sec FLOAT8 NOT NULL,
	msg_source UUID NOT NULL,
	CONSTRAINT "primary" PRIMARY KEY (id ASC)
);

GRANT SELECT, INSERT ON TABLE tgthr.balances TO paymentauth;
GRANT SELECT ON TABLE tgthr.balances TO chewpaca;

CREATE TABLE IF NOT EXISTS tgthr.plaidtokens (
	created_at TIMESTAMPTZ NULL DEFAULT now():::TIMESTAMPTZ,
	token STRING NOT NULL,
	user_id UUID NOT NULL,
	revision INT8 NOT NULL,
	account_primary STRING NULL,
	account_primary_account STRING NULL,
	account_primary_routing STRING NULL,
	plaid_environment STRING NOT NULL,
	msg_source UUID NOT NULL,
	plaid_item_id STRING NOT NULL DEFAULT 'NIL':::STRING,
	CONSTRAINT "primary" PRIMARY KEY (user_id ASC, revision DESC)
);

GRANT SELECT, INSERT ON TABLE tgthr.plaidtokens TO paymentauth;
GRANT SELECT ON TABLE tgthr.plaidtokens TO chewpaca;
