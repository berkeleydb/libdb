---
title: "set_timeout"
api-name: "set_timeout"
source: docs/api_reference/C/set_timeout_parameter.html
---
## set_timeout

Sets timeout values for locks or transactions in the database environment, and the wait time for a process to exit the environment when DB_REGISTER recovery is needed.

The syntax for setting timeout value for database environment's lock, before recovery is started, and transaction is as follows:

- DB_SET_LOCK_TIMEOUT

  Configures the database environment's lock timeout value. The syntax of the entry in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_lock_timeout`, one or more whitespace characters, and the lock timeout value.

- DB_SET_REG_TIMEOUT

  Sets the timeout value on how long to wait for processes to exit the environment before recovery is started. The syntax of the entry in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_reg_timeout`, one or more whitespace characters, and the wait timeout value.

- DB_SET_TXN_TIMEOUT

  Sets the timeout value for transactions in this database environment. The syntax of the entry in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_txn_timeout`, one or more whitespace characters, and the transaction timeout value

For more information, see <a href="envset_timeout.md" class="xref" title="DB_ENV-&gt;set_timeout()">DB_ENV-&gt;set_timeout()</a>.
