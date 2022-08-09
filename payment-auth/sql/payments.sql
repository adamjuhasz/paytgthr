CREATE TABLE IF NOT EXISTS tgthr.payments (
  id UUID NOT NULL,
	revision INT8 NOT NULL,
	version STRING NOT NULL,
	created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
	msg_source UUID NOT NULL,
	status STRING NOT NULL,
	transaction_id UUID NULL,
	user_id UUID NOT NULL,
	pay_type STRING NOT NULL,
	pay_method STRING NOT NULL,
	pay_method_id STRING NULL,
	pay_amount JSONB NOT NULL,
	description STRING NOT NULL,
	"visible" BOOLEAN NOT NULL,
	pay_subtype JSONB NOT NULL DEFAULT '"InitialVerification"',
	pay_achinfo JSONB NULL,
	pay_failurecode JSONB NULL,
	CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC),
	INVERTED INDEX amount_idx (pay_amount),
	UNIQUE INDEX cant_repeat_state (id ASC, status ASC)
);
GRANT select, insert ON TABLE tgthr.payments TO paymentauth;
GRANT select ON TABLE tgthr.payments TO chewpaca;

DROP VIEW IF EXISTS tgthr.payments_collapsed;
CREATE VIEW 
	tgthr.payments_collapsed
	AS SELECT DISTINCT ON (id) id, revision, version, msg_source, status, transaction_id, user_id, pay_type, pay_method, pay_amount, description, pay_method_id, created_at, "visible", pay_subtype, pay_achinfo, pay_failurecode   
	FROM tgthr.public.payments ORDER BY id DESC, revision DESC;

GRANT SELECT ON tgthr.payments_collapsed to paymentauth;
GRANT SELECT ON tgthr.payments_collapsed to chewpaca;