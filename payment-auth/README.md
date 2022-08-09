# payment-auth

Authorize payments based on user info

## Expected Environmental Variables

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
  - `host=localhost port=26257 dbname=tgthr connect_timeout=10 sslmode=require user=guest password=guest application_name='payment-auth'`
- Create
  ```
  $ echo -en "host=armadillo-cockroachdb-public port=26257 dbname=tgthr connect_timeout=10 sslmode=require user=paymentauth password=paymentauth application_name='landing-page'" > psql_conn_string
  ```

### PLAID_CLIENT_ID

- **Required**
- Example
  - `5c58dade13822600115851ea`
- Create
  ```
  $ echo -en "5c58dade13822600115851ea" > plaid_client_id
  ```

### PLAID_SANDBOX_SECRET

- **Required** // **Secret**
- Example
  - `****`
- Create
  ```
  $ echo -en "****" > plaid_secret
  ```

### PLAID_SANDBOX_URL

- **Required**
- Example
  - `https://development.plaid.com`
- Create
  ```
  $ echo -en "https://development.plaid.com" > plaid_base_url
  ```

### PLAID_DEV_SECRET

- **Required** // **Secret**
- Example
  - `****`
- Create
  ```
  $ echo -en "****" > plaid_secret
  ```

### PLAID_DEV_URL

- **Required**
- Example
  - `https://development.plaid.com`
- Create
  ```
  $ echo -en "https://development.plaid.com" > plaid_base_url
  ```

### PLAID_PROD_URL

- **Required** // **Secret**
- Example
  - `****`
- Create
  ```
  $ echo -en "****" > plaid_secret
  ```

### PLAID_PROD_SECRET

- **Required**
- Example
  - `https://development.plaid.com`
- Create
  ```
  $ echo -en "https://development.plaid.com" > plaid_base_url
  ```

```
$ kubectl create secret generic payment-auth-secret \
  --from-file psql_conn_string \
  --from-file amqp_conn_string \
  --from-file plaid_client_id \
  --from-file plaid_secret \
  --from-file plaid_base_url
```

## Services required

- Database
- RabbitMQ
- AccountsFSM

## Expected Database Table(s)

### Plaid Access Tokens

#### Purpose

Store access tokens for linked users

#### Structure

```sql
CREATE TABLE tgthr.plaidtokens (
	created_at TIMESTAMPTZ NOT NULL DEFAULT now():::TIMESTAMPTZ,
	token STRING NOT NULL,
  user_id UUID NOT NULL,
  revision INT NOT NULL,
  account_primary STRING NULL,
  account_primary_routing STRING NULL,
  account_primary_account STRING NULL,
  plaid_environment STRING NOT NULL,
  msg_source UUID NOT NULL,
  plaid_item_id STRING NOT NULL DEFAULT ('NIL'),
	CONSTRAINT "primary" PRIMARY KEY (user_id ASC, revision DESC)
);
GRANT ALL ON TABLE tgthr.plaidtokens TO paymentauth;
```

### Retrieved Balances

#### Purpose

Store balances we have retrirved

#### Structure

[SQL File](./sql/balances.sql)

## Expected RabbitMQ Queue(s)

### paymentauth

#### Purpose

Handle Commands for payment auth

#### Bindings

- tgthr
  - payment.cmd.#

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
