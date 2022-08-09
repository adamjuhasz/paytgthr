path "kv/data/privacy-api/*" {
  capabilities = ["read"]
}

path "kv/data/ssn_transit/*" {
  capabilities = ["read", "list"]  
}

path "kv/*" {
  capabilities = ["list"]
}

path "transit/encrypt/pin_encryption" {
  capabilities = ["read", "update"]
}
path "transit/decrypt/pin_encryption" {
  capabilities = ["read", "update"]
}