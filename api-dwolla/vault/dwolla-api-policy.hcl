path "rabbitmq/creds/generalrw-role" {
  capabilities = ["read"]
}

path "kv/data/dwolla-api/*" {
  capabilities = ["read"]
}