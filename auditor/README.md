# Auditor

Log & Audit actions taken on the system

## Expected Environmental Variables

### ISPROD

- **Optional**
- "true" if running in production environment

### PSQL_STRING

- **Required** // **Secret**
- Connection string to connect to the correct postgres or cockroackdb server, should contain
  - Hostname
  - Port
  - User
  - Password
  - Database
  - Application name
- Example
  - `host=localhost port=26257 dbname=tgthr connect_timeout=10 sslmode=require user=password password=password application_name='auditor'`
- Create
  ```
  $ echo -en "host=armadillo-cockroachdb-public port=26257 dbname=tgthr connect_timeout=10 sslmode=require user=password password=password application_name='landing-page'" > psql_conn_string
  ```

## Services required

- Database
- RabbbitMQ

## Expected Database Table(s)

### audit_log

#### Purpose

Store all messages sent through RabbitMQ

#### Structure

```sql
CREATE TABLE tgthr.audit_log (
  msg_id UUID PRIMARY KEY,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
  msg_timestamp TIMESTAMPTZ NOT NULL,
  parent_id UUID NULL,
  source_id UUID NULL,
  msg_type TEXT NULL,
  msg_version TEXT NULL,
  body_key TEXT NULL,
  body_contents JSONB NULL
);
GRANT ALL ON TABLE tgthr.audit_log TO auditor;
```

### Expected RabbitMQ Queue(s)

### auditlog

#### Purpose

Get a copy of all messages

### Bindings

- tgthr
  - #

## Development

### Building

```
$ stack build --pedantic
```

### Testing

```
$ stack test
```

### Running

```
$ PSQL_STRING="host=localhost port=26257 dbname=landingpage connect_timeout=10 sslmode=require user=password password=adam application_name='landing-page'" stack run
```
