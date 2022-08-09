# Chewpaca

Customer service tool

## SQL

```sql
CREATE USER 'chewpaca' WITH PASSWORD 'password';
GRANT SELECT ON tgthr.* TO chewpaca;
```

## SECRET

```
$ kubectl create secret generic chewpaca-secret --from-literal="psql_conn_string=host=armadillo-cockroachdb-public port=26257 dbname=tgthr connect_timeout=10 sslmode=require user=chewpaca password=password application_name='chewpaca'"
```
