# landing-page

Our user facing marketing page

## Expected Environmental Variables

### SESSION_KEY

- **Required** // **Secret**
- Used to initialize the cookie encryption

### ISPROD

- Optional
- "true" if running in production environment

```
$ vault write auth/kubernetes/role/landing-page \
        bound_service_account_names=landing-page \
        bound_service_account_namespaces=default \
        policies=landing-page-policy,ssn-encrypt-decrypt,default \
        ttl=5m
```

## RMQ_ROLE

- **Required**
- Used to get RMQ credentials from Vault

## AMQP_BASE

- **Required**
- Used to connect to AMQP

## PORT

## Services required

- RabbbitMQ

## Expected RabbitMQ Exchange(s)

tgthr

## Expected RabbitMQ Queue(s)

none

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
$ cat .env
verify existence
$ stack run
```
