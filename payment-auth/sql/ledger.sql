CREATE TABLE IF NOT EXISTS tgthr.ledger (
  id UUID NOT NULL DEFAULT gen_random_uuid(),
	revision INT8 NOT NULL,
	version STRING NOT NULL,
	msg_source UUID NOT NULL,
	user_id UUID NOT NULL,
	balance JSONB NOT NULL,
	type STRING NOT NULL,
	type_id STRING NULL,
	amount JSONB NOT NULL,
	idempotency STRING NULL,
	created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
	CONSTRAINT "primary" PRIMARY KEY (user_id ASC, revision DESC),
	UNIQUE INDEX enforce_unique_id (id ASC),
	INDEX ledger_user_id_created_at_idx (user_id ASC, created_at DESC)
);

GRANT SELECT, INSERT ON tgthr.ledger to paymentauth;
GRANT SELECT ON tgthr.ledger to chewpaca;