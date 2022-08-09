CREATE TABLE IF NOT EXISTS tgthr.transactions (
  id UUID NOT NULL,
	revision INT8 NOT NULL,
	version STRING NOT NULL,
	created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
	msg_source UUID NOT NULL,
	state STRING NOT NULL,
	dispute_info JSONB NULL,
	decline_info JSONB NULL,
	source STRING NOT NULL,
	source_id STRING NOT NULL,
	source_event STRING NOT NULL,
	user_id UUID NOT NULL,
	amount_display JSONB NOT NULL,
	amount_billing JSONB NOT NULL,
	details JSONB NULL,
	group_id UUID NULL,
	group_revision INT8 NULL,
	source_idempotency STRING NULL,
	split_amounts JSONB NULL,
	merchant JSONB NULL,
	description STRING NULL,
	purchased_at TIMESTAMPTZ NOT NULL,
	adjustments JSONB NOT NULL,
	CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC),
	UNIQUE INDEX transactions_source_idempotency_key (source_idempotency ASC),
	UNIQUE INDEX external_idempotent (source_idempotency DESC),
	INVERTED INDEX dispute_idx (dispute_info),
	INVERTED INDEX decline_idx (decline_info),
	INVERTED INDEX amount_display_idx (amount_display),
	INVERTED INDEX billing_idx (amount_billing),
	INVERTED INDEX details_idx (details),
	INVERTED INDEX split_idx (split_amounts),
	INVERTED INDEX merchant_idx (merchant)
);

GRANT select, insert ON TABLE tgthr.transactions TO paymentauth;
GRANT select ON TABLE tgthr.transactions TO chewpaca;

DROP VIEW IF EXISTS tgthr.transactions_collapsed;
CREATE VIEW tgthr.transactions_collapsed (id, revision, version, msg_source, state, dispute_info, decline_info, source, source_id, source_event, user_id, amount_display, amount_billing, details, group_id, group_revision, source_idempotency, split_amounts, merchant, description, purchased_at, adjustments) AS SELECT DISTINCT ON (id) id, revision, version, msg_source, state, dispute_info, decline_info, source, source_id, source_event, user_id, amount_display, amount_billing, details, group_id, group_revision, source_idempotency, split_amounts, merchant, description, purchased_at, adjustments FROM tgthr.public.transactions ORDER BY id DESC, revision DESC;
GRANT select ON TABLE tgthr.transactions_collapsed TO paymentauth;
GRANT select ON TABLE tgthr.transactions_collapsed TO chewpaca;

DROP VIEW IF EXISTS tgthr.transactions_byuserid;
CREATE VIEW tgthr.transactions_byuserid (id, user_id, created_at, purchased_at) AS SELECT id, user_id, created_at, purchased_at FROM (SELECT id::UUID, (jsonb_array_elements(split_amounts)->>0)::UUID AS user_id, created_at, purchased_at FROM (SELECT DISTINCT ON (id) id, revision, split_amounts, created_at, purchased_at FROM tgthr.public.transactions ORDER BY id DESC, revision DESC));
GRANT select ON TABLE tgthr.transactions_byuserid TO paymentauth;
GRANT select ON TABLE tgthr.transactions_byuserid TO chewpaca;