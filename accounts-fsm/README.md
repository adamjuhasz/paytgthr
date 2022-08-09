# accounts-fsm

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
  - `host=localhost port=26257 dbname=tgthr connect_timeout=10 sslmode=require user=password password=password application_name='accounts-fsm'`
- Create
  ```
  $ echo -en "host=armadillo-cockroachdb-public port=26257 dbname=tgthr connect_timeout=10 sslmode=require user=password password=password application_name='landing-page'" > psql_conn_string
  ```

### AMQP_STRING

- **Required** // **Secret**
- Connection string to connect to the correct rabbitMQ cluster, should contain
  - Hostname
  - Port
  - User
  - Password
  - vhost
- Example
  - `amqp://guest:guest@localhost:5672/`
- Create
  ```
  $ echo -en "amqp://guest:guest@rhinelander-rabbitmq-ha:5672/" > amqp_conn_string
  ```

```
$ kubectl create secret generic accounts-fsm-secret \
  --from-file psql_conn_string \
  --from-file amqp_conn_string
```

## Services required

- Database
- RabbbitMQ

## Expected Database Table(s)

### users

#### Purpose

Store all messages sent through RabbitMQ

#### Structure

[SQL File - tgthr.users](sql/users.sql)

```sql
GRANT select, insert ON TABLE tgthr.users TO accounts;
```

## groups

#### Purpose

#### Structure

[SQL File - tgthr.groups](sql/groups.sql)

```sql
GRANT select, insert ON TABLE tgthr.groups TO accounts;
```

### Expected RabbitMQ Queue(s)

### accounts

#### Purpose

- Accepts commands

### Bindings

- tgthr
  - account.cmd.#

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
