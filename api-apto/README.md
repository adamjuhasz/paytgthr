# api-apto

Webhooks from Apto converted to events

## Expected Environmental Variables

### AMQP_STRING

- **Required** // **Secret**
- Connection string to connect to the correct rabbitMQ cluster, should contain
  - Hostname
  - Port
  - User
  - Password
  - vhost
- Example
  - `amqp://aptoapi:password@localhost:5672/`
- Create
  ```
  $ echo -en "amqp://aptoapi:password@rhinelander-rabbitmq-ha:5672/" > amqp_conn_string
  ```

## Secrets

```
$ vault write auth/kubernetes/role/apto-api \
        bound_service_account_names=apto-api \
        bound_service_account_namespaces=default \
        policies=api-apto-generic,ssn-encrypt-decrypt,default \
        ttl=720h
```

## Services required

- RabbbitMQ

## Development

### Building

```
$ stack build --pedantic
```

### Testing

```
$ stack test --pedantic
```

### Running

```
$ stack run
```

### Profiling

```
$ stack build --profile && stack exec -- api-apto-exe +RTS -p -h
```
