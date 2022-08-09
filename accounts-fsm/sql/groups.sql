CREATE TABLE IF NOT EXISTS tgthr.groups (
	id UUID NOT NULL,
	status STRING NOT NULL,
	created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
	revision INT8 NOT NULL,
	version STRING NOT NULL,
	time_start TIMESTAMPTZ NULL,
	time_end TIMESTAMPTZ NULL,
	split JSONB NOT NULL,
	members JSONB NOT NULL,
	msg_source UUID NOT NULL,
	CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC),
	UNIQUE INDEX one_msg_one_group (id ASC, msg_source ASC),
	INVERTED INDEX groups_split_idx (split),
	INDEX groups_id_revision_status_idx (id ASC, revision DESC, status ASC),
	INVERTED INDEX groups_split_idx1 (split),
	INDEX groups_id_revision_status_idx1 (id ASC, revision DESC, status ASC) STORING (split)
);

GRANT SELECT ON tgthr.groups TO chewpaca;
GRANT SELECT, INSERT ON tgthr.groups TO accounts;
