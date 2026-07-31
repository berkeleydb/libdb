---
title: "DB_ENV->set_tx_recover"
api-name: "DB_ENV->set_tx_recover"
source: docs/upgrading/upgrade_3_2_tx_recover.html
---
## DB_ENV-\>set_tx_recover

The **info** parameter of the function passed to DB_ENV-\>set_tx_recover is no longer needed. If your application calls DB_ENV-\>set_tx_recover, find the callback function referred to by that call and remove the **info** parameter.

In addition, the called function no longer needs to handle Berkeley DB log records, Berkeley DB will handle them internally as well as call the application-specified function. Any handling of Berkeley DB log records in the application's callback function may be removed.

In addition, the callback function will no longer be called with the <a href="../../api/c/envset_app_dispatch.md#set_app_dispatch_DB_TXN_FORWARD_ROLL" class="olink">DB_TXN_FORWARD_ROLL</a> flag specified unless the transaction enclosing the operation successfully committed.
