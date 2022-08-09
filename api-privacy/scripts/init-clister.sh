vault policy write privacy-api-policy ../vault/privacy-api-policy.hcl \
&& vault write auth/kubernetes/role/privacy-api bound_service_account_names=privacy-api bound_service_account_namespaces=default policies=privacy-api-policy,ssn-encrypt-decrypt,default ttl=5m
