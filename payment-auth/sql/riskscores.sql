CREATE TABLE IF NOT EXISTS tgthr.riskscores (
  id UUID NOT NULL DEFAULT gen_random_uuid(),
	user_id UUID NOT NULL,
	revision INT8 NOT NULL,
  score FLOAT NOT NULL,
  change FLOAT NOT NULL,
  fact JSONB NOT NULL,
  msg_source UUID NOT NULL,
	created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
	CONSTRAINT "primary" PRIMARY KEY (user_id ASC, revision DESC),
	UNIQUE INDEX enforce_unique_id (id ASC),
	INDEX ledger_user_id_created_at_idx (user_id ASC, created_at DESC)
);

GRANT SELECT, INSERT ON tgthr.riskscores to paymentauth;
GRANT SELECT ON tgthr.riskscores to chewpaca;

DROP VIEW IF EXISTS tgthr.riskscores_collapsed;
CREATE VIEW tgthr.riskscores_collapsed AS SELECT DISTINCT ON (user_id) user_id, revision, score, change, fact, msg_source, created_at FROM tgthr.riskscores ORDER BY user_id ASC, revision DESC;
GRANT SELECT ON tgthr.riskscores_collapsed to paymentauth;
GRANT SELECT ON tgthr.riskscores_collapsed to chewpaca;