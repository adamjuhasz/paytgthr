vault kv put kv/dwolla-api/dwolla-token client_id=id client_secret=secret webhook_secret=secret \
&& vault policy write dwolla-api-policy ../vault/dwolla-api-policy.hcl \
&& vault write auth/kubernetes/role/dwolla-api bound_service_account_names=dwolla-api bound_service_account_namespaces=default policies=dwolla-api-policy,default ttl=5m
