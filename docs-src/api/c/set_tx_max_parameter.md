---
title: "set_tx_max"
api-name: "set_tx_max"
source: docs/api_reference/C/set_tx_max_parameter.html
---
## set_tx_max

Configures the Berkeley DB database environment to support at least the minimum number of simultaneously active transactions supported by Berkeley DB database environment. This value bounds the size of the memory allocated for transactions. Child transactions are counted as active until they either commit or abort.

The syntax of this parameter in the <a href="../../programmer_reference/env_db_config.html#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> file is a single line with the string `set_tx_max`, one or more whitespace characters, and the number of transactions.

For more information, see <a href="envset_tx_max.md" class="xref" title="DB_ENV-&gt;set_tx_max()">DB_ENV-&gt;set_tx_max()</a>.
