CREATE SEQUENCE IF NOT EXISTS tgthr.links_id_seq START 1 INCREMENT 1;
CREATE TABLE IF NOT EXISTS tgthr.links (
  id INT PRIMARY KEY DEFAULT nextval('tgthr.links_id_seq'),
  type JSONB NOT NULL,
  expiration TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ
);
GRANT SELECT, INSERT ON tgthr.links TO landingpage;
GRANT SELECT, UPDATE ON tgthr.links_id_seq TO landingpage;