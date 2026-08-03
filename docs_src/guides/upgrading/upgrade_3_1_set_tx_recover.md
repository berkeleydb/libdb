---
title: "DB_ENV->set_tx_recover"
api-name: "DB_ENV->set_tx_recover"
source: docs/upgrading/upgrade_3_1_set_tx_recover.html
---
## DB_ENV-\>set_tx_recover

The redo parameter of the function passed to DB_ENV-\>set_tx_recover used to be an integer set to any one of a number of \#defined values. In the 3.1 release of Berkeley DB, the redo parameter has been replaced by the op parameter which is an enumerated type of type db_recops.

If your application calls DB_ENV-\>set_tx_recover, then find the function referred to by the call. Replace the flag values in that function as follows:

| Previous flag     | Berkeley DB 3.1 version flag |
|-------------------|------------------------------|
| TXN_BACKWARD_ROLL | DB_TXN_BACKWARD_ROLL         |
| TXN_FORWARD_ROLL  | DB_TXN_FORWARD_ROLL          |
| TXN_OPENFILES     | DB_TXN_OPENFILES             |
| TXN_REDO          | DB_TXN_FORWARD_ROLL          |
| TXN_UNDO          | DB_TXN_ABORT                 |
