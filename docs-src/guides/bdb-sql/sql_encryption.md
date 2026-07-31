---
title: "Encryption"
api-name: "Encryption"
source: docs/bdb-sql/sql_encryption.html
---
## Encryption

The Berkeley DB SQL interface supports the SQLite Encryption Extension (SEE) to ensure security of your data. The supported encryption algorithm is AES-128 in CBC mode. For more information on the concepts relating to BDB encryption, see the <a href="../../guides/programmer_reference/env_encrypt.md" class="olink">Berkeley DB Programmer's Reference Guide.</a>

To learn how to use the SQLite Encryption Extension (SEE), see the official <a href="http://www.hwaci.com/sw/sqlite/see.html" class="ulink" target="_top">SQLite Documentation Page.</a>

### Note

The Berkeley DB SQL interface does not support the sqlite3_rekey method.
