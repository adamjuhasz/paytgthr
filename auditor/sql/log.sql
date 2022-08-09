CREATE TABLE IF NOT EXISTS tgthr.audit_log (
	msg_id UUID NOT NULL,
	created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
	msg_timestamp TIMESTAMPTZ NOT NULL,
	parent_id UUID NULL,
	source_id UUID NULL,
	msg_type STRING NULL,
	msg_version STRING NULL,
	body_key STRING NULL,
	body_contents JSONB NULL,
	CONSTRAINT "primary" PRIMARY KEY (msg_id ASC),
	INDEX audit_log_msg_id_msg_timestamp_body_key_idx (msg_id ASC, msg_timestamp DESC, body_key ASC)
);

GRANT SELECT ON tgthr.audit_log TO chewpaca;
GRANT SELECT, INSERT ON tgthr.audit_log TO auditor;
