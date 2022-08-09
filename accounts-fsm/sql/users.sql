CREATE TABLE IF NOT EXISTS tgthr.users (
  id UUID NOT NULL,
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
	CONSTRAINT "primary" PRIMARY KEY (id ASC, revision DESC),
	UNIQUE INDEX one_msg_one_user (id DESC, msg_source DESC),
	INDEX users_id_revision_created_at_idx (id ASC, revision DESC, created_at DESC)
);

GRANT SELECT, INSERT ON tgthr.users TO chewpaca;
GRANT SELECT, INSERT ON tgthr.users TO accounts;

DROP VIEW IF EXISTS tgthr.users_collapsed;
CREATE VIEW tgthr.users_collapsed AS SELECT DISTINCT ON (id) id, revision, version, msg_source, email, password, name_first, name_last, address_street, address_street2, address_city, address_state, address_zip, bank_routing, bank_account, phone_number, dob, ssn, disclosure_accept, consent_accept, apto_cardholderid, apto_kyc_status, apto_cardid, apto_cardstatus, status, closure_reason, bank_nickname, bank_name, dwolla_customerid, dwolla_fundingid, created_at, bank_type, bank_verified, bank_verified_amounts, created_on, first_signin_on, activated_on, card_created_on, card_activated_on FROM tgthr.users ORDER BY id DESC, revision DESC;

GRANT SELECT ON tgthr.users_collapsed TO chewpaca;
GRANT SELECT ON tgthr.users_collapsed TO accounts;
GRANT SELECT ON tgthr.users_collapsed TO paymentauth;

