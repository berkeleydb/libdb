---
title: "DB_CHKSUM_SHA1"
api-name: "DB_CHKSUM_SHA1"
source: docs/upgrading/upgrade_4_2_cksum.html
---
## DB_CHKSUM_SHA1

The flag to enable checksumming of Berkeley DB databases pages was renamed from DB_CHKSUM_SHA1 to <a href="../../api/c/dbset_flags.md#dbset_flags_DB_CHKSUM" class="olink">DB_CHKSUM</a>, as Berkeley DB uses an internal function to generate hash values for unencrypted database pages, not the SHA1 Secure Hash Algorithm. Berkeley DB continues to use the SHA1 Secure Hash Algorithm to generate hashes for encrypted database pages. Applications using the DB_CHKSUM_SHA1 flag should change that use to <a href="../../api/c/dbset_flags.md#dbset_flags_DB_CHKSUM" class="olink">DB_CHKSUM</a>; no other change is required.
