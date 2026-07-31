---
title: "DB_ENV->set_server"
api-name: "DB_ENV->set_server"
source: docs/upgrading/upgrade_4_0_rpc.html
---
## DB_ENV-\>set_server

`The DB_ENV->set_server()` method has been replaced with the `DB_ENV->set_rpc_server()` method. The `DB_ENV->set_server()` method can be easily converted to the `DB_ENV->set_rpc_server()` method by changing the name, and specifying a NULL for the added argument, second in the argument list.
