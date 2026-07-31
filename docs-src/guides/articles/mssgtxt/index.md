---
title: "Berkeley DB Message Reference for Stripped Libraries"
api-name: "Berkeley DB Message Reference for Stripped Libraries"
source: docs/articles/mssgtxt/index.html
---
# Berkeley DB Message Reference for Stripped Libraries

**Legal Notice**

This documentation is distributed under an open source license. You may review the terms of this license at: <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/oslicense-093458.html</a>

Oracle, Berkeley DB, and Sleepycat are trademarks or registered trademarks of Oracle. All rights to these marks are reserved. No third-party use is permitted without the express prior written consent of Oracle.

Other names may be trademarks of their respective owners.

To obtain a copy of this document's original source code, please submit a request to the Oracle Technology Network forum at: <a href="http://forums.oracle.com/forums/forum.jspa?forumID=271" class="ulink" target="_top">http://forums.oracle.com/forums/forum.jspa?forumID=271</a>

9/9/2013

------------------------------------------------------------------------

**Table of Contents**

<span class="sect1"> [Introduction](index.md#intro) </span>

<span class="sect1"> [Access Methods Messages](index.md#am) </span>

<span class="sect1"> [Common Messages](index.md#common) </span>

<span class="sect1"> [Database Handle Messages](index.md#db) </span>

<span class="sect1"> [Environment Handle Messages](index.md#env) </span>

<span class="sect1"> [Locking Subsystem Messages](index.md#lock) </span>

<span class="sect1"> [Logging Subsystem Messages](index.md#log) </span>

<span class="sect1"> [Memory Pool Messages](index.md#mpool) </span>

<span class="sect1"> [Replication Messages](index.md#rep) </span>

<span class="sect1"> [Sequences Messages](index.md#sequence) </span>

<span class="sect1"> [Transaction Messages](index.md#txn) </span>

<span class="sect1"> [Command Line Utilities Messages](index.md#util) </span>

## Introduction

When you build Berkeley DB, it is possible to cause all the error message text to be stripped from the library. This is done so as to minimize the library's footprint on devices where memory and storage is severely constrained.

How you strip error messages from your library depends on the platform you are building on. For \*nix platforms, you do this when <a href="../../../guides/installation/build_unix_conf.md" class="olink">you configure your build</a> using the <a href="../../../guides/installation/build_unix_conf.md#build_unix_conf.--enable-stripped_messages" class="olink">--enable-stripped_messages</a> configuration option. Your messages will also be stripped if you <a href="../../../guides/installation/build_unix_small.md" class="olink">configure for small builds.</a>

For Windows, you can enable stripped messages using the <a href="../../../guides/installation/win_additional_options.md#HAVE_STRIPPED_MESSAGES" class="olink">HAVE_STRIPPED_MESSAGES</a> build property. Your messages will also be stripped if you <a href="../../../guides/installation/build_win_small.md" class="olink">build a small memory footprint library.</a>

Stripped libraries still issue error messages, but the only thing displayed is the error number — the text of the message is not available for the library to display. This document provides the missing error message text.

Message text is organized into tables, where each table identifies a specific portion of the library. Each such table is then sorted by message number.

The areas of the library which can issue messages are:

- Access method. These are error messages related to usage of the various access methods (Btree, Heap, Queue, and so forth).

- Common error messages. These are messages that can be commonly issued by any area of the library.

- Database error messages. These are error messages related to the usage of database handles.

- Environment error messages. These are error messages related to the usage of environment handles.

- Locking subsystem error messages.

- Logging subsystem error messages.

- Memory pool error messages.

- Replication error messages. These error messages are issued by methods specific to either Base API or Replication Manager applications.

- Sequences error messages.

- Transactions error messages.

- Command line utility error messages.

## Access Methods Messages

| Message Number | Message Text |
|----|----|
| 1001 | illegal record number size |
| 1002 | illegal record number of 0 |
| 1003 | %s: write failed to backing file |
| 1004 | Existing data sorts differently from put data |
| 1005 | cursor adjustment after delete failed |
| 1006 | prefix comparison may not be specified for default comparison routine |
| 1007 | bt_minkey value of %lu too high for page size of %lu |
| 1008 | %s: btree version %lu requires a version upgrade |
| 1009 | %s: unsupported btree version: %lu |
| 1010 | %s: DB_DUP specified to open method but not set in database |
| 1011 | %s: DB_RECNUM specified to open method but not set in database |
| 1012 | %s: DB_FIXEDLEN specified to open method but not set in database |
| 1013 | %s: DB_RENUMBER specified to open method but not set in database |
| 1014 | %s: multiple databases specified but not supported by file |
| 1015 | %s: duplicate sort specified but not supported in database |
| 1016 | %s: compresssion specified to open method but not set in database |
| 1017 | %s: compression support has not been compiled in |
| 1018 | open method type is Btree, database type is Recno |
| 1019 | open method type is Recno, database type is Btree |
| 1020 | Not enough room in parent: %s: page %lu |
| 1021 | Too many btree levels: %d |
| 1022 | Unknown record format, page %lu, indx 0 |
| 1023 | Compact cannot handle zero length key |
| 1024 | DB_RECNUM cannot be used with compression |
| 1025 | DB_DUP cannot be used with compression without DB_DUPSORT |
| 1026 | compression support has not been compiled in |
| 1027 | compression cannot be used with DB_RECNUM |
| 1028 | compression cannot be used with DB_DUP without DB_DUPSORT |
| 1029 | to enable compression you need to supply both function arguments |
| 1030 | compression support has not been compiled in |
| 1031 | minimum bt_minkey value is 2 |
| 1032 | Existing data sorts differently from put data |
| 1033 | Both cursors must be initialized before calling DBC-\>cmp. |
| 1034 | Page %lu: nonsensical bt_minkey value %lu on metadata page |
| 1035 | Page %lu: nonsensical root page %lu on metadata page |
| 1036 | Page %lu: Btree metadata page has both duplicates and multiple databases |
| 1037 | Page %lu: Btree metadata page illegally has both recnums and dups |
| 1038 | Page %lu: metadata page has renumber flag set but is not recno |
| 1039 | Page %lu: Btree metadata page illegally has both recnums and compression |
| 1040 | Page %lu: Btree metadata page illegally has both ""unsorted duplicates and compression |
| 1041 | Page %lu: recno metadata page specifies duplicates |
| 1042 | Page %lu: re_len of %lu in non-fixed-length database |
| 1043 | Page %lu: Recno database has dups |
| 1044 | Page %lu: nonsensical type for item %lu |
| 1045 | Page %lu: item order check unsafe: skipping |
| 1046 | Page %lu: entries listing %lu overlaps data |
| 1047 | Page %lu: bad offset %lu at index %lu |
| 1048 | Page %lu: RINTERNAL structure at offset %lu referenced twice |
| 1049 | Page %lu: gap between items at offset %lu |
| 1050 | Page %lu: bad HOFFSET %lu, appears to be %lu |
| 1051 | Page %lu: duplicated item %lu |
| 1052 | Page %lu: duplicated item %lu |
| 1053 | Page %lu: item %lu marked deleted |
| 1054 | Page %lu: duplicate page referenced by internal btree page at item %lu |
| 1055 | Page %lu: duplicate page referenced by recno page at item %lu |
| 1056 | Page %lu: impossible tlen %lu, item %lu |
| 1057 | Page %lu: offpage item %lu has bad pgno %lu |
| 1058 | Page %lu: item %lu of invalid type %lu |
| 1059 | Page %lu: gap between items at offset %lu |
| 1060 | Page %lu: offset %lu unaligned |
| 1061 | Page %lu: overlapping items at offset %lu |
| 1062 | Page %lu: overlapping items at offset %lu |
| 1063 | Page %lu: bad HOFFSET %lu, appears to be %lu |
| 1064 | Page %lu: lowest key on internal page of nonzero length |
| 1065 | Page %lu: error %lu in fetching overflow item %lu |
| 1066 | Page %lu: out-of-order key at entry %lu |
| 1067 | Page %lu: non-dup dup key at entry %lu |
| 1068 | Page %lu: database with no duplicates has duplicated keys |
| 1069 | Page %lu: btree metadata page observed twice |
| 1070 | Page %lu: btree metadata page has no root |
| 1071 | Page %lu: recno database has bad re_len %lu |
| 1072 | Page %lu: duplicate tree referenced from metadata page |
| 1073 | Page %lu: btree root of incorrect type %lu on metadata page |
| 1074 | Page %lu: unexpected page type %lu found in leaf chain (expected %lu) |
| 1075 | Page %lu: incorrect next_pgno %lu found in leaf chain (should be %lu) |
| 1076 | Page %lu: incorrect prev_pgno %lu found in leaf chain (should be %lu) |
| 1077 | Page %lu: recno leaf page non-recno tree |
| 1078 | Page %lu: non-recno leaf page in recno tree |
| 1079 | Page %lu: duplicates in non-dup btree |
| 1080 | Page %lu: unsorted duplicate set in sorted-dup database |
| 1081 | Page %lu: btree or recno page is of inappropriate type %lu |
| 1082 | Page %lu: recno page returned bad re_len %lu |
| 1083 | Page %lu: record count incorrect: actual %lu, in record %lu |
| 1084 | Page %lu: recno level incorrect: got %lu, expected %lu |
| 1085 | Page %lu: overflow page %lu referenced more than twice from internal page |
| 1086 | Page %lu: item %lu has incorrect record count of %lu, should be %lu |
| 1087 | Page %lu: Btree level incorrect: got %lu, expected %lu |
| 1088 | Page %lu: internal page is empty and should not be |
| 1089 | Page %lu: bad record count: has %lu records, claims %lu |
| 1090 | Page %lu: linked twice |
| 1091 | Page %lu: unterminated leaf chain |
| 1092 | Page %lu: first item on page sorted greater than parent entry |
| 1093 | Page %lu: first item on page had comparison error |
| 1094 | Page %lu: last item on page sorted greater than parent entry |
| 1095 | Page %lu: last item on page had comparison error |
| 1096 | Page %lu: database has custom hash function; reverify with DB_NOORDERCHK set |
| 1097 | Page %lu: Impossible max_bucket %lu on meta page |
| 1098 | Page %lu: incorrect high_mask %lu, should be %lu |
| 1099 | Page %lu: incorrect low_mask %lu, should be %lu |
| 1100 | Page %lu: suspiciously high nelem of %lu |
| 1101 | Page %lu: spares array entry %d is invalid |
| 1102 | Page %lu: item %lu is out of order or nonsensical |
| 1103 | Page %lu: entries array collided with data |
| 1104 | Page %lu: hash key stored as duplicate item %lu |
| 1105 | Page %lu: duplicate item %lu has bad length |
| 1106 | Page %lu: duplicate item %lu has two different lengths |
| 1107 | Page %lu: offpage item %lu has bad pgno %lu |
| 1108 | Page %lu: offpage item %lu has bad page number |
| 1109 | Page %lu: item %lu has bad type |
| 1110 | Page %lu: Hash meta page referenced twice |
| 1111 | Page %lu: hash bucket %lu maps to non-hash page |
| 1112 | Page %lu: non-empty page in unused hash bucket %lu |
| 1113 | Page %lu: above max_bucket referenced |
| 1114 | Page %lu: impossible first page in bucket %lu |
| 1115 | Page %lu: first page in hash bucket %lu has a prev_pgno |
| 1116 | Page %lu: hash page referenced twice |
| 1117 | Page %lu: duplicates present in non-duplicate database |
| 1118 | Page %lu: unsorted dups in sorted-dup database |
| 1119 | Page %lu: hash page has bad next_pgno |
| 1120 | Page %lu: hash page has bad prev_pgno |
| 1121 | Page %lu: item %lu hashes incorrectly |
| 1122 | Invalid flag in \_\_ham_curadj_recover |
| 1123 | Cannot replicate prepared transactions from master running release 4.2. |
| 1124 | %s: Invalid hash meta page %lu |
| 1125 | %s: hash version %lu requires a version upgrade |
| 1126 | %s: unsupported hash version: %lu |
| 1127 | %s: DB_DUP specified to open method but not set in database |
| 1128 | %s: multiple databases specified but not supported in file |
| 1129 | %s: duplicate sort function specified but not set in database |
| 1130 | H_NOMORE returned to \_\_hamc_get |
| 1131 | Existing data sorts differently from put data |
| 1132 | Attempt to return a deleted item |
| 1133 | library build did not include support for the Hash access method |
| 1134 | Extent size may not be specified for in-memory queue database |
| 1135 | Multiversion queue databases are not supported |
| 1136 | \_\_qam_open: %s: unexpected file type or format |
| 1137 | %s: queue version %lu requires a version upgrade |
| 1138 | %s: unsupported qam version: %lu |
| 1139 | Record size of %lu too large for page size of %lu |
| 1140 | Extent size must be at least 1 |
| 1141 | Queue does not support multiple databases per file |
| 1142 | Record length error: data offset plus length larger than record size of %lu |
| 1143 | illegal record number size |
| 1144 | illegal record number of 0 |
| 1145 | library build did not include support for the Queue access method |
| 1146 | Page %lu: queue databases must be one-per-file |
| 1147 | Page %lu: queue record length %lu too high for page size and recs/page |
| 1148 | Page %lu: database contains multiple Queue metadata pages |
| 1149 | Warning: %d extra extent files found |
| 1150 | Page %lu: queue record %lu extends past end of page |
| 1151 | Page %lu: queue record %lu has bad flags (%#lx) |
| 1152 | Page %lu: queue database has no meta page |
| 1153 | Page %lu: queue database page of incorrect type %lu |
| 1154 | Page %lu: queue database page of incorrect type %lu |
| 1155 | %s: specified heap size does not match size set in database |
| 1156 | Page %lu: Heap databases must be one-per-file |
| 1157 | Page %lu: Number of heap regions incorrect |
| 1158 | Page %lu: last_pgno beyond end of fixed size heap |
| 1159 | Page %lu: incorrect number of entries in page's offset table |
| 1160 | Page %lu: record %lu (length %lu) overlaps next record |
| 1161 | Page %lu: record %lu (length %lu) beyond end of page |
| 1162 | Page %lu: heap database has no meta page |
| 1163 | Page %lu: heap database page of incorrect type %lu |
| 1164 | Page %lu: heap database missing region page (page type %lu) |
| 1165 | Page %lu: record %lu has invalid flags |
| 1166 | Page %lu heap database page beyond high page in region |
| 1167 | Incorrect record size in header: %s: rid %lu.%lu |
| 1168 | region size may not be 0 |
| 1169 | region size may not be larger than %lu |
| 1170 | The key/data pairs in the buffer are not sorted. |
| 1171 | The key/data pairs in the buffer are not sorted. |
| 1172 | The DBT items in the buffer are not sorted |

## Common Messages

| Message Number | Message Text |
|----|----|
| 0001 | fcntl(F_SETFD) |
| 0002 | \_\_fop_file_setup: Retry limit (%d) exceeded |
| 0003 | Transactional create on replication client disallowed |
| 0004 | fop_read_meta: %s: unexpected file type or format |
| 0005 | rename: file %s exists |
| 0040 | Encrypted environment: library build did not include cryptography support |
| 0041 | unsupported byte order, only big and little-endian supported |
| 0042 | %s: %s: Invalid numeric argument |
| 0043 | %s: Invalid numeric argument |
| 0044 | %s: %s: Less than minimum value (%ld) |
| 0045 | %s: Less than minimum value (%ld) |
| 0046 | %s: %s: Greater than maximum value (%ld) |
| 0047 | %s: Greater than maximum value (%ld) |
| 0048 | %s: %s: Invalid numeric argument |
| 0049 | %s: Invalid numeric argument |
| 0050 | %s: %s: Less than minimum value (%lu) |
| 0051 | %s: Less than minimum value (%lu) |
| 0052 | %s: %s: Greater than maximum value (%lu) |
| 0053 | %s: Greater than maximum value (%lu) |
| 0054 | illegal flag combination specified to %s |
| 0055 | illegal flag specified to %s |
| 0056 | %s: DB_READ_COMMITTED, DB_READ_UNCOMMITTED and DB_RMW require locking |
| 0057 | unable to create/retrieve page %lu |
| 0058 | page %lu: illegal page type or format |
| 0059 | assert failure: %s/%d: "%s" |
| 0060 | PANIC: fatal region error detected; run recovery |
| 0061 | PANIC |
| 0062 | Successful return: 0 |
| 0063 | DB_BUFFER_SMALL: User memory too small for return value |
| 0064 | DB_DONOTINDEX: Secondary index callback returns null |
| 0065 | DB_FOREIGN_CONFLICT: A foreign database constraint has been violated |
| 0066 | DB_KEYEMPTY: Non-existent key/data pair |
| 0067 | DB_KEYEXIST: Key/data pair already exists |
| 0068 | DB_LOCK_DEADLOCK: Locker killed to resolve a deadlock |
| 0069 | DB_LOCK_NOTGRANTED: Lock not granted |
| 0070 | DB_LOG_BUFFER_FULL: In-memory log buffer is full |
| 0071 | DB_LOG_VERIFY_BAD: Log verification failed |
| 0072 | DB_NOSERVER: No message dispatch call-back function has been configured |
| 0073 | DB_NOTFOUND: No matching key/data pair found |
| 0074 | DB_OLDVERSION: Database requires a version upgrade |
| 0075 | DB_PAGE_NOTFOUND: Requested page not found |
| 0076 | DB_REP_DUPMASTER: A second master site appeared |
| 0077 | DB_REP_HANDLE_DEAD: Handle is no longer valid |
| 0078 | DB_REP_HOLDELECTION: Need to hold an election |
| 0079 | DB_REP_IGNORE: Replication record/operation ignored |
| 0080 | DB_REP_ISPERM: Permanent record written |
| 0081 | DB_REP_JOIN_FAILURE: Unable to join replication group |
| 0082 | DB_REP_LEASE_EXPIRED: Replication leases have expired |
| 0083 | DB_REP_LOCKOUT: Waiting for replication recovery to complete |
| 0084 | DB_REP_NEWSITE: A new site has entered the system |
| 0085 | DB_REP_NOTPERM: Permanent log record not written |
| 0086 | DB_REP_UNAVAIL: Too few remote sites to complete operation |
| 0087 | DB_RUNRECOVERY: Fatal error, run database recovery |
| 0088 | DB_SECONDARY_BAD: Secondary index inconsistent with primary |
| 0089 | DB_TIMEOUT: Operation timed out |
| 0090 | DB_VERIFY_BAD: Database verification failed |
| 0091 | DB_VERSION_MISMATCH: Database environment version mismatch |
| 0092 | Unknown error: %d |
| 0093 | %s: Unknown flag: %#x |
| 0094 | %s: Unexpected database type: %s |
| 0095 | %s: Unexpected code path error |
| 0096 | Read-only transaction cannot be used for an update |
| 0097 | Transaction not specified for a transactional database |
| 0098 | Transaction specified for a non-transactional database |
| 0099 | Operation forbidden while secondary index is being created |
| 0100 | Transaction and database from different environments |
| 0101 | Transaction that opened the DB handle is still active |
| 0102 | %s%sprevious transaction deadlock return not resolved |
| 0103 | DB environment not configured for transactions |
| 0104 | %lu larger than database's maximum record length %lu |
| 0105 | Record length error: ""replacement length %lu differs from replaced length %lu |
| 0106 | dbc_logging: Client update |
| 0107 | Dbc_logging: Master non-txn update |
| 0108 | Rep: flags 0x%lx msg_th %lu |
| 0109 | Rep: handle %lu, opcnt %lu |
| 0110 | Log sequence error: page LSN %lu %lu; previous LSN %lu %lu |
| 0111 | %s: attempt to modify a read-only database |
| 0112 | %s: file limited to %lu pages |
| 0113 | Thread/process %s failed: %s |
| 0114 | architecture does not support locks inside system shared memory |
| 0115 | no base system shared memory ID specified |
| 0116 | shmget: key: %ld: shared system memory region already exists |
| 0117 | shmget: key: %ld: unable to create shared system memory region |
| 0118 | shmat: id %d: unable to attach to shared system memory region |
| 0119 | shmctl/SHM_LOCK: id %d: unable to lock down shared memory region |
| 0120 | architecture lacks mmap(2), shared environments not possible |
| 0121 | shmdt |
| 0122 | shmctl: id %d: unable to delete system shared memory region |
| 0123 | munmap |
| 0124 | fileops: munmap |
| 0125 | fileops: mmap %s |
| 0126 | mmap |
| 0127 | mlock |
| 0128 | architecture doesn't support environments in system memory |
| 0129 | fileops: mkdir %s |
| 0130 | fileops: read %s: %lu bytes at offset %lu |
| 0131 | fileops: write %s: %lu bytes at offset %lu |
| 0132 | fileops: read %s: %lu bytes |
| 0133 | read: %#lx, %lu |
| 0134 | read: %#lx, %lu |
| 0135 | fileops: write %s: %lu bytes |
| 0136 | write: %#lx, %lu |
| 0137 | write: %#lx, %lu |
| 0138 | fileops: flock %s %s offset %lu |
| 0139 | fcntl |
| 0140 | advisory file locking unavailable |
| 0141 | fileops: truncate %s to %lu |
| 0142 | ftruncate: %lu |
| 0143 | malloc: %lu |
| 0144 | user-specified malloc function returned NULL |
| 0145 | realloc: %lu |
| 0146 | User-specified realloc function returned NULL |
| 0147 | malloc: %lu |
| 0148 | realloc: %lu |
| 0149 | Guard byte incorrect during free |
| 0150 | fileops: flush %s |
| 0151 | fsync |
| 0152 | fileops: open %s |
| 0153 | %s(%u): host lookup failed: %s |
| 0154 | %s(%u): host lookup failed |
| 0155 | %s(%u): host lookup failed: %s |
| 0156 | %s(%u): host lookup failed: %d |
| 0157 | %s: buffer too small to hold environment variable %s |
| 0158 | stat: %s |
| 0159 | fileops: directory list %s |
| 0160 | fileops: unlink %s |
| 0161 | unlink: %s |
| 0162 | fcntl(F_SETFD) |
| 0163 | fileops: close %s |
| 0164 | close |
| 0165 | fileops: stat %s |
| 0166 | fstat |
| 0167 | select |
| 0168 | fileops: rename %s to %s |
| 0169 | rename %s %s |
| 0170 | fileops: seek %s to %lu |
| 0171 | seek: %lu: (%lu \* %lu) + %lu |
| 0172 | Joining non-encrypted environment with encryption key |
| 0173 | Encryption algorithm not supplied |
| 0174 | Encrypted environment: no encryption key supplied |
| 0175 | Invalid password |
| 0176 | Environment encrypted using a different algorithm |
| 0177 | No cipher structure given |
| 0178 | Encrypted database: no encryption flag specified |
| 0179 | Database encrypted using a different algorithm |
| 0180 | Invalid password |
| 0181 | Unencrypted database with a supplied encryption key |
| 0182 | IPP AES NULL pointer error |
| 0183 | IPP AES length error |
| 0184 | IPP AES context does not match operation |
| 0185 | IPP AES srclen size error |
| 0186 | AES key direction is invalid |
| 0187 | AES key material not of correct length |
| 0188 | AES key passwd not valid |
| 0189 | AES cipher in wrong state (not initialized) |
| 0190 | AES bad block length |
| 0191 | AES cipher instance is invalid |
| 0192 | AES data contents are invalid |
| 0193 | AES unknown error |
| 0194 | AES error unrecognized |
| 0195 | Unencrypted checksum with a supplied encryption key |
| 0196 | Encrypted checksum: no encryption key specified |
| 0197 | segment %s does not exist |
| 0198 | no base shared memory ID specified |
| 0199 | key: %ld: shared memory region already exists |
| 0200 | shared memory segment already exists |
| 0201 | shared memory segment not initialized |
| 0202 | shared memory segment not initialized |
| 0203 | no segment name given |
| 0204 | Invalid segment id given |
| 0205 | shared memory segment not initialized |
| 0206 | segment id %ld out of range |
| 0207 | DB_REP_WOULDROLLBACK: Client data has diverged |
| 0208 | DB_HEAP_FULL: no free space in db |

## Database Handle Messages

| Message Number | Message Text |
|----|----|
| 0501 | "Page %lu: %s is of inappropriate type %lu |
| 0502 | "Page %lu: totally zeroed page |
| 0503 | Rename on temporary files invalid |
| 0504 | XA applications may not specify an environment to db_create |
| 0505 | Cannot open XA database before XA is enabled |
| 0506 | call implies an access method which is inconsistent with previous calls |
| 0507 | Directory %s not in environment list. |
| 0508 | Database environment not configured for encryption |
| 0509 | page sizes may not be smaller than %lu |
| 0510 | page sizes may not be larger than %lu |
| 0511 | page sizes must be a power-of-2 |
| 0512 | Illegal application-specific record type %lu in log |
| 0513 | Illegal record type %lu in log |
| 0514 | Attempting to add application-specific record with invalid type %lu |
| 0515 | Attempting to add internal record with invalid type %lu |
| 0516 | DB_DBT_PARTIAL may not be set on key during join_get |
| 0517 | Allocation failed for join key, len = %lu |
| 0518 | DB_SALVAGE requires a an output handle |
| 0519 | DB_ORDERCHKONLY requires a database name |
| 0520 | Metadata page %lu cannot be read |
| 0521 | Page %lu: Incomplete metadata page |
| 0522 | Page %lu: metadata page corrupted |
| 0523 | Page %lu: could not check metadata page |
| 0524 | Page %lu: pgno incorrectly set to %lu |
| 0525 | Page %lu: bad magic number %lu |
| 0526 | Page %lu: unsupported DB version %lu; extraneous errors may result |
| 0527 | Page %lu: bad page size %lu |
| 0528 | Page %lu: bad page type %lu |
| 0529 | Page %lu: bad meta-data flags value %#lx |
| 0530 | Page %lu: beyond the end of the file, metadata page has last page as %lu |
| 0531 | Page %lu: old-style duplicate page |
| 0532 | Page %lu: unknown page type %lu |
| 0533 | Page %lu: overflow refcount %lu, referenced %lu times |
| 0534 | Page %lu: unreferenced page |
| 0535 | Page %lu: totally zeroed page |
| 0536 | Page %lu: bad page number %lu |
| 0537 | Page %lu: bad page type %lu |
| 0538 | Page %lu: invalid next_pgno %lu |
| 0539 | Page %lu: invalid prev_pgno %lu |
| 0540 | Page %lu: invalid next_pgno %lu |
| 0541 | Page %lu: too many entries: %lu |
| 0542 | Page %lu: bad btree level %lu |
| 0543 | Page %lu: btree leaf page has incorrect level %lu |
| 0544 | Page %lu: nonzero level %lu in non-btree database |
| 0545 | Page %lu: invalid magic number |
| 0546 | Page %lu: magic number does not match database type |
| 0547 | Page %lu: unsupported database version %lu; extraneous errors may result |
| 0548 | Page %lu: invalid pagesize %lu |
| 0549 | Page %lu: bad meta-data flags value %#lx |
| 0550 | Page %lu: nonempty free list on subdatabase metadata page |
| 0551 | Page %lu: nonsensical free list pgno %lu |
| 0552 | Page %lu: last_pgno is not correct: %lu != %lu |
| 0553 | Page %lu: invalid next_pgno %lu on free list page |
| 0554 | Page %lu: page %lu encountered a second time on free list |
| 0555 | Page %lu: non-invalid page %lu on free list |
| 0556 | Subdatabase entry not page-number size |
| 0557 | Subdatabase entry references invalid page %lu |
| 0558 | Subdatabase entry references page %lu of invalid type %lu |
| 0559 | Subdatabase entry of invalid size |
| 0560 | Page %lu: DB-\>h_internal field is NULL |
| 0561 | Page %lu: incorrect hash function for database |
| 0562 | Page %lu: database metapage of bad type %lu |
| 0563 | Page %lu: entries listing %lu overlaps data |
| 0564 | Page %lu: bad offset %lu at page index %lu |
| 0565 | Page %lu: unaligned offset %lu at page index %lu |
| 0566 | Page %lu: item %lu of unrecognizable type |
| 0567 | Page %lu: item %lu extends past page boundary |
| 0568 | Page %lu: sorted duplicate set in unsorted-dup database |
| 0569 | Page %lu: unsorted duplicate set in sorted-dup database |
| 0570 | Page %lu: duplicate page of inappropriate type %lu |
| 0571 | library build did not include support for database verification |
| 0572 | Databases may not become secondary indices while cursors are open |
| 0573 | Secondary index handles may not be re-associated |
| 0574 | Secondary indices may not be used as primary databases |
| 0575 | Primary databases may not be configured with duplicates |
| 0576 | Renumbering recno databases may not be used as primary databases |
| 0577 | The primary and secondary must be opened in the same environment |
| 0578 | The DB_THREAD setting must be the same for primary and secondary |
| 0579 | Callback function may be NULL only when database handles are read-only |
| 0580 | replication recovery unrolled committed transactions; |
| 0581 | DB-\>del with DB_MULTIPLE(\_KEY) requires multiple key records |
| 0582 | Database does not have a valid file handle |
| 0583 | %s is not supported with DB_CONSUME or DB_CONSUME_WAIT |
| 0584 | DB_DBT_READONLY should not be set on data DBT. |
| 0585 | DB_MULTIPLE requires DB_DBT_USERMEM be set |
| 0586 | DB_MULTIPLE does not support DB_DBT_PARTIAL |
| 0587 | DB_MULTIPLE buffers must be aligned, |
| 0588 | At least one secondary cursor must be specified to DB-\>join |
| 0589 | All secondary cursors must share the same transaction |
| 0590 | files containing multiple databases may only be opened read-only |
| 0591 | DB_TRUNCATE not supported on VxWorks |
| 0592 | DB_UNKNOWN type specified with DB_CREATE or DB_TRUNCATE |
| 0593 | unknown type: %lu |
| 0594 | database environment not yet opened |
| 0595 | environment did not include a memory pool |
| 0596 | environment not created using DB_THREAD |
| 0597 | DB_MULTIVERSION illegal without a transaction specified |
| 0598 | DB_MULTIVERSION illegal with queue databases |
| 0599 | DB_TRUNCATE illegal with %s specified |
| 0600 | Queue databases must be one-per-file |
| 0601 | DB-\>pget may only be used on secondary indices |
| 0602 | DB_MULTIPLE and DB_MULTIPLE_KEY may not be used on secondary indices |
| 0603 | DB_GET_BOTH on a secondary index requires a primary key |
| 0604 | DB-\>put forbidden on secondary indices |
| 0605 | DB-\>put: DB_MULTIPLE(\_KEY) can only be combined with DB_OVERWRITE_DUP |
| 0606 | DB-\>put with DB_MULTIPLE(\_KEY) requires a bulk key buffer |
| 0607 | DB-\>put with DB_MULTIPLE requires a bulk data buffer |
| 0608 | a partial put in the presence of duplicates requires a cursor operation |
| 0609 | DB-\>compact may not be called with active cursors in the transaction. |
| 0610 | Secondary indices may not be used as foreign databases |
| 0611 | Foreign databases may not be configured with duplicates |
| 0612 | Renumbering recno databases may not be used as foreign databases |
| 0613 | The associating database must be a secondary index. |
| 0614 | When specifying a delete action of nullify, a callback |
| 0615 | When not specifying a delete action of nullify, a |
| 0616 | Closing already-closed cursor |
| 0617 | DBcursor-\>cmp dbc pointer must not be null |
| 0618 | DBcursor-\>cmp both cursors must refer to the same database. |
| 0619 | DB_READ_UNCOMMITTED is not supported with DB_CONSUME or DB_CONSUME_WAIT |
| 0620 | DB_DBT_READONLY should not be set on data DBT. |
| 0621 | DB_MULTIPLE/DB_MULTIPLE_KEY require DB_DBT_USERMEM be set |
| 0622 | DB_MULTIPLE/DB_MULTIPLE_KEY do not support DB_DBT_PARTIAL |
| 0623 | DB_MULTIPLE/DB_MULTIPLE_KEY buffers must be |
| 0624 | DBcursor-\>pget may only be used on secondary indices |
| 0625 | DB_MULTIPLE and DB_MULTIPLE_KEY may not be used on secondary indices |
| 0626 | %s requires both a secondary and a primary key |
| 0627 | DB_GET_BOTH on a secondary index requires a primary key |
| 0628 | DBcursor-\>put forbidden on secondary indices |
| 0629 | Bulk and partial operations cannot be combined on %s DBT |
| 0630 | DB_THREAD mandates memory allocation flag on %s DBT |
| 0631 | Cursor position must be set before performing this operation |
| 0632 | DB_AUTO_COMMIT may not be specified along with a transaction handle |
| 0633 | DB_AUTO_COMMIT may not be specified in non-transactional environment |
| 0634 | Partitioned databases may not be in memory. |
| 0635 | DB_CREATE must be specified to create databases. |
| 0636 | DBTYPE of unknown without existing file |
| 0637 | Partitioned databases may not be included with multiple databases. |
| 0638 | %s: Invalid type %d specified |
| 0639 | Invalid subdatabase type %d specified |
| 0640 | %s: metadata page checksum error |
| 0641 | \_\_db_meta_setup: %s: unexpected file type or format |
| 0642 | Checksum failure requires catastrophic recovery |
| 0643 | Cannot replicate prepared transactions from master running release 4.2 |
| 0644 | Partition open failed to allocate %d bytes |
| 0645 | Cannot specify callback and range keys. |
| 0646 | Must specify at least 2 partitions. |
| 0647 | Must specify either keys or a callback. |
| 0648 | May not specify both keys and a callback. |
| 0649 | Directory not in environment list %s |
| 0650 | Partitioning may only specified on BTREE and HASH databases. |
| 0651 | Partitioning specified on a non-partitioned database. |
| 0652 | Incompatible partitioning specified. |
| 0653 | Partition callback not specified. |
| 0654 | Record numbers are not supported in partitioned databases. |
| 0655 | Zero paritions specified. |
| 0656 | Number of partitions does not match. |
| 0657 | Hash database must specify a partition callback. |
| 0658 | Partitioning only supported on BTREE nad HASH. |
| 0659 | No range keys found. |
| 0660 | Keys found and callback set. |
| 0661 | Partition key 0 is not empty. |
| 0662 | Partition key %d does not match |
| 0663 | A partitioned database can not be in a multiple databases file |
| 0664 | library build did not include support for the database partitioning |
| 0665 | upgrade not supported |
| 0666 | %s: unsupported btree version: %lu |
| 0667 | Attempt to upgrade an encrypted database without providing a password. |
| 0668 | %s: unsupported hash version: %lu |
| 0669 | %s: unsupported queue version: %lu |
| 0670 | %s: DB-\>upgrade only supported on native byte-order systems |
| 0671 | %s: unrecognized file type |
| 0672 | %s: file size not a multiple of the pagesize |
| 0673 | rename: database %s exists |
| 0674 | Closing a primary DB while a secondary DB has active cursors is unsafe |
| 0675 | \_\_env_fileid_reset: %s: unexpected file type or format |
| 0676 | Page %lu: overflow page has zero reference count |
| 0677 | Page %lu: overflow page of invalid type %lu |
| 0678 | Page %lu: first page in overflow chain has a prev_pgno %lu |
| 0679 | Page %lu: encountered too many times in overflow traversal |
| 0680 | Page %lu: overflow page linked twice from leaf or data page |
| 0681 | Page %lu: bad next_pgno %lu on overflow page |
| 0682 | Page %lu: bad prev_pgno %lu on overflow page (should be %lu) |
| 0683 | Page %lu: overflow item incomplete |
| 0684 | checksum error: page %lu: catastrophic recovery required |
| 0685 | DB-\>truncate forbidden on secondary indices |
| 0686 | DB-\>truncate not permitted with active cursors |
| 0687 | CDS groups do not support %s |
| 0688 | CDS group has active cursors |
| 0689 | %s page %lu is on free list with type %lu |
| 0690 | DB_LOG_NO_DATA may not be specified within a transaction. |
| 0691 | Remove on temporary files invalid |
| 0692 | Both cursors must be initialized before calling DBC-\>cmp. |
| 0693 | Both cursors must be initialized before calling DBC-\>cmp. |
| 0694 | DBCursor-\>cmp mismatched off page duplicate cursor pointers. |
| 0695 | Put results in a non-unique secondary key in an |
| 0696 | Duplicate data items are not supported with sorted data |
| 0697 | Write attempted on read-only cursor |
| 0698 | Attempt to execute cascading delete in a foreign index failed |
| 0699 | Foreign database application callback |
| 0700 | Attempt to overwrite item in foreign database with nullified value failed |
| 0701 | DB_PRIVATE is not supported by |
| 0702 | Deadlock while opening %s, retrying |
| 0708 | Invalid positioning flag combined with DB_DBT_PARTIAL |
| 0709 | The primary key returned by pget can't be partial |
| 0710 | Invalid positioning flag combined with DB_DBT_PARTIAL |
| 0711 | The primary key returned by pget can't be partial. |
| 0713 | Page %lu: page %lu on free list beyond last_pgno %lu |
| 0714 | Metadata page %lu cannot be read from mpool |
| 0715 | removing %s |
| 0716 | Target directory may not be null. |
| 0717 | %s: path too long |
| 0718 | %s: directory read |
| 0719 | highest numbered log file removed: %d |
| 0720 | %s: path too long |
| 0721 | %s: cannot create |
| 0722 | %s: path too long |
| 0723 | %s: directory read |
| 0724 | copying database %s%c%s to %s%c%s |
| 0725 | data directory '%s' is absolute path, not permitted unless backup is to a single directory |
| 0726 | copying %s to %s |
| 0727 | %lu buffer allocation |
| 0728 | %s: path too long |
| 0729 | %s%c%s not present |
| 0731 | Sync failed |
| 0732 | %s: path too long |
| 0733 | %s: path too long |
| 0734 | %s: cannot create |
| 0735 | Can't flush log |
| 0736 | Can't get log file names |
| 0737 | %s: path too long |
| 0738 | %s: path too long |
| 0739 | moving %s to %s |
| 0740 | removing %s |
| 0741 | unlink of %s failed |
| 0742 | lowest numbered log file copied: %d |
| 0743 | the largest log file removed (%d) must be greater than or equal the smallest log file copied (%d) |
| 0745 | Write failed. |
| 0746 | Exclusive database handles cannot be threaded. |
| 0747 | Exclusive database handles require transactional environments. |
| 0748 | Exclusive database handles cannot be opened on replication clients. |

## Environment Handle Messages

| Message Number | Message Text |
|----|----|
| 1501 | Logging region out of memory; you may need to increase its size |
| 1502 | Freeing log information for process: %s, (ref %lu) |
| 1503 | DB_ENV-\>failchk requires DB_ENV-\>is_alive be configured |
| 1504 | is_alive method specified but no thread region allocated |
| 1505 | thread table must be allocated when the database environment is created |
| 1506 | unable to allocate a thread status block |
| 1507 | Thread died in Berkeley DB library |
| 1508 | Unable to allocate thread control block |
| 1509 | Invalid recovery timestamp %s; earliest time is %s |
| 1510 | First log record not found |
| 1511 | Invalid checkpoint record at \[%ld\]\[%ld\] |
| 1512 | Last log record not found |
| 1513 | Checkpoint LSN record \[%ld\]\[%ld\] not found |
| 1514 | Recovery starting from \[%lu\]\[%lu\] |
| 1515 | Recovery continuing after non-fatal checkpoint error: %s |
| 1516 | First log record not found |
| 1517 | Invalid checkpoint record at \[%ld\]\[%ld\] |
| 1518 | Recovery complete at %.24s |
| 1519 | Maximum transaction ID %lx recovery checkpoint \[%lu\]\[%lu\] |
| 1520 | Recovery function for LSN %lu %lu failed on %s pass |
| 1521 | Recovery function for LSN %lu %lu failed |
| 1522 | Log file corrupt at LSN: \[%lu\]\[%lu\] |
| 1523 | Unknown version %lu |
| 1524 | %lu: register environment |
| 1525 | %lu: creating %s |
| 1526 | %lu: adding self to registry |
| 1527 | %02u: EMPTY |
| 1528 | DB_REGISTER limits processes to one open DB_ENV handle per environment |
| 1529 | %02u: %s: KILLED |
| 1530 | %02u: %s: FAILED |
| 1531 | %02u: %s: LOCKED |
| 1532 | %lu: locking slot %02u at offset %lu |
| 1533 | %lu: recovery completed, unlocking |
| 1534 | %s: exclusive file unlock |
| 1535 | %s: existing environment not created in system memory |
| 1536 | %s: unable to read region info |
| 1537 | %s: unable to read system-memory information |
| 1538 | Program version %d.%d doesn't match environment version %d.%d |
| 1539 | Build signature doesn't match environment |
| 1540 | configured environment flags incompatible with existing environment |
| 1542 | Minimum environment memory size %ld is bigger than spcified max %ld. |
| 1543 | unable to create new master region array |
| 1544 | %s: unable to find environment |
| 1545 | %s: unable to write out public environment ID |
| 1546 | unable to join the environment |
| 1547 | environment reference count went negative |
| 1548 | region size %lu is too large; maximum is %lu |
| 1549 | region max %lu is too large; maximum is %lu |
| 1550 | architecture does not support locks inside process-local (malloc) memory |
| 1551 | application may not specify both DB_PRIVATE and DB_THREAD |
| 1552 | region memory was not correctly aligned |
| 1553 | no room remaining for additional REGIONs |
| 1554 | Library build did not include statistics support |
| 1555 | library build did not include support for cryptography |
| 1556 | Empty password specified to set_encrypt |
| 1557 | library build did not include support for cryptography |
| 1558 | Environment panic set |
| 1559 | DB_TXN_NOSYNC and DB_TXN_WRITE_NOSYNC |
| 1560 | Attempt to decrement hotbackup counter past zero |
| 1561 | Directory %s not in environment list. |
| 1562 | is_alive method specified but no thread region allocated |
| 1563 | is_alive method specified but no thread region allocated |
| 1564 | %s: method not permitted when environment specified |
| 1565 | %s: method not permitted %s handle's open method |
| 1566 | %s interface requires an environment configured for the %s subsystem |
| 1567 | The DB_RECOVER flag was not specified, and recovery is needed |
| 1568 | Berkeley DB library does not support DB_REGISTER on this system |
| 1569 | registration requires transaction support |
| 1570 | Berkeley DB library does not support replication on this system |
| 1571 | replication requires locking support |
| 1572 | replication requires transaction support |
| 1573 | recovery requires the create flag |
| 1574 | recovery requires transaction support |
| 1575 | DB_FAILCHK requires DB_ENV-\>is_alive be configured |
| 1576 | DB_FAILCHK requires DB_ENV-\>set_thread_count be configured |
| 1577 | Berkeley DB library configured to support only private environments |
| 1578 | architecture lacks fast mutexes: applications cannot be threaded |
| 1579 | Database handles still open at environment close |
| 1580 | Open database handle: %s%s%s |
| 1581 | File handles still open at environment close |
| 1582 | Open file handle: %s |
| 1583 | block not at end of region |
| 1584 | line %d: %s: incorrect name-value pair |
| 1585 | unrecognized name-value pair: %s |
| 1586 | temporary open: %s |
| 1587 | %s interface requires an environment configured with %s |
| 1588 | Maximum memory size too large: maximum is 4GB |
| 1589 | DB_PRIVATE is not |
| 1590 | Could not add %s to environment list. |

## Locking Subsystem Messages

| Message Number | Message Text |
|----|----|
| 2001 | library build did not include support for mutexes |
| 2002 | Win32 create event failed |
| 2003 | Win32 lock failed: mutex already locked by %s |
| 2004 | \[%I64d\]: Lost signal on mutex %p, ""id %d, ms %d |
| 2005 | \[%I64d\]: Waiting on mutex %p, id %d |
| 2006 | Win32 lock failed |
| 2007 | \[%I64d\]: Lost signal on mutex %p, ""id %d, ms %d |
| 2008 | \[%I64d\]: Waiting on mutex %p, id %d |
| 2009 | Win32 read lock failed |
| 2010 | Win32 unlock failed: lock already unlocked: mutex %d busy %d |
| 2011 | \[%I64d\]: Signalling mutex %p, id %d |
| 2012 | Win32 unlock failed |
| 2013 | Unable to allocate memory for the mutex region |
| 2014 | Unable to allocate memory for mutexes from the region |
| 2015 | Unable to acquire/release a mutex; check configuration |
| 2016 | Unable to acquire/release a shared latch; check configuration |
| 2017 | Freeing mutex for process: %s |
| 2018 | DB_ENV-\>mutex_set_align: alignment value must be a non-zero power-of-two |
| 2019 | fcntl lock failed |
| 2020 | fcntl unlock failed: lock already unlocked |
| 2021 | unable to initialize mutex |
| 2022 | pthread lock failed: lock currently in use: pid/tid: %s |
| 2023 | pthread lock failed |
| 2024 | pthread readlock failed |
| 2025 | pthread unlock failed: lock already unlocked |
| 2026 | unable to destroy cond |
| 2027 | unable to destroy mutex |
| 2028 | TAS: mutex not appropriately aligned |
| 2029 | TAS: mutex initialize |
| 2030 | TAS lock failed: lock %ld currently in use: ID: %s |
| 2031 | shared unlock %ld already unlocked |
| 2032 | unlock %ld already unlocked |
| 2033 | Mutex allocated before mutex region. |
| 2034 | unable to allocate memory for mutex; resize mutex region |
| 2035 | Invalid lock operation: %d |
| 2036 | Locker does not exist |
| 2037 | DB_ENV-\>lock_get: invalid lock mode %lu |
| 2038 | Unexpected lock status: %d |
| 2039 | Not a child transaction |
| 2040 | Locker does not exist |
| 2041 | lock_open: incompatible deadlock detector mode |
| 2042 | unable to allocate memory for the lock table |
| 2043 | DB_ENV-\>set_lk_detect: unknown deadlock detection mode specified |
| 2044 | DB_ENV-\>set_lk_detect: incompatible deadlock detector mode |
| 2045 | Unknown locker id: %lx |
| 2046 | Locker still has locks |
| 2047 | Freeing locker with locks |
| 2048 | DB_ENV-\>lock_detect: unknown deadlock detection mode specified |
| 2049 | warning: unable to abort locker %lx |
| 2050 | Aborting locker %lx |
| 2051 | %lu lockers |
| 2052 | locker has write locks |
| 2053 | Freeing read locks for locker %#lx: %s |
| 2054 | library build did not include support for locking |
| 2055 | Lock table is out of available %s |

## Logging Subsystem Messages

| Message Number | Message Text |
|----|----|
| 2501 | Set either an lsn range or a time range to verify logs |
| 2502 | \[%lu\]\[%lu\] Unsupported version of log file, ""log file number: %u, log file version: %u, ""supported log version: %u. |
| 2503 | callback: initialization |
| 2504 | Log verification ended and %s. |
| 2505 | Not supported version %lu |
| 2506 | file %s has LSN %lu/%lu, past end of log at %lu/%lu |
| 2507 | Commonly caused by moving a database from one database environment |
| 2508 | to another without clearing the database LSNs, or by removing all of |
| 2509 | the log files from a database environment |
| 2510 | Logging not currently permitted |
| 2511 | DB_ENV-\>log_put is illegal on replication clients |
| 2512 | Non-replication DB_ENV handle attempting |
| 2513 | DB_ENV-\>log_put: record larger than maximum file size (%lu \> %lu) |
| 2514 | Write failed on MASTER commit. |
| 2515 | Short read while restoring log |
| 2516 | DB_ENV-\>log_flush: LSN of %lu/%lu past current end-of-log of %lu/%lu |
| 2517 | Database environment corrupt; the wrong log files may |
| 2518 | DB_ENV-\>log_file is illegal with in-memory logs |
| 2519 | DB_ENV-\>log_file: name buffer is too short |
| 2520 | %s: log file unreadable |
| 2521 | %s: log file open failed |
| 2522 | DB_ENV-\>log_put is illegal on replication clients |
| 2523 | library build did not include support for log verification |
| 2524 | unable to allocate log region memory |
| 2525 | No log files found |
| 2526 | Finding last valid log LSN: file: %lu offset %lu |
| 2527 | Invalid log file: %s |
| 2528 | ignoring log file: %s |
| 2529 | Ignoring log file: %s historic byte order |
| 2530 | Ignoring log file: %s: magic number %lx, not %lx |
| 2531 | Unacceptable log file %s: unsupported log version %lu |
| 2532 | Skipping log file %s: historic log version %lu |
| 2533 | log record checksum mismatch |
| 2534 | Warning: truncating to point beyond end of log |
| 2535 | In-memory log buffer is full (an active transaction spans the buffer) |
| 2536 | "\[%lu\]\[%lu\] Not supported type of log record %u. |
| 2537 | \[%lu\]\[%lu\] \[WARNING\] Parent txn %lx is updating its ""active child txn %lx's pages, or %lx aborted. |
| 2538 | \[%lu\]\[%lu\] \[WARNING\] Txn %lx is updating txn %lx's pages. |
| 2539 | \[%lu\]\[%lu\] Verifying log record of type %s |
| 2540 | \[%lu\]\[%lu\] Log record type does not match related database type, ""current database type: %s, expected database type according to ""the log record type: %s. |
| 2541 | \[%lu\]\[%lu\] Suspicious dbreg operation: %s, the ""database file %s's register in log region does ""not begin with an open operation. |
| 2542 | \[%lu\]\[%lu\] Wrong dbreg operation ""sequence, opening %s for id %d which is already ""open. |
| 2543 | \[%lu\]\[%lu\] Wrong dbreg operation sequence,""file %s with id %d is first seen of ""status: %s |
| 2544 | \[%lu\]\[%lu\] The dbtype of database file %s with uid %s "" and id %d has changed from %s to %s. |
| 2545 | \[%lu\]\[%lu\] Wrong dbreg operation sequence for file %s ""with id %d, current status: %s, new status: %s |
| 2546 | \[%lu\]\[%lu\] \_\_ham_groupalloc should apply only to the ""master database with meta page number 0, current meta ""page number is %d. |
| 2547 | \[%lu\]\[%lu\] Can not find an active transaction's ""information, txnid: %lx. |
| 2548 | \[%lu\]\[%lu\] The number of active, committed and aborted ""child txns of txn %lx: %u, %u, %u. |
| 2549 | \[%lu\]\[%lu\] Checkpoint record, ckp_lsn: \[%lu\]\[%lu\], ""timestamp: %s. Total checkpoint: %u |
| 2550 | \[%lu\]\[%lu\] Last known checkpoint \[%lu\]\[%lu\] not equal ""to last_ckp :\[%lu\]\[%lu\]. Some checkpoint log records ""may be missing. |
| 2551 | \[%lu\]\[%lu\] Last known checkpoint \[%lu, %lu\] has a ""timestamp %s smaller than this checkpoint timestamp %s. |
| 2552 | \[%lu\]\[%lu\] ckp log's ckp_lsn \[%lu\]\[%lu\] greater than ""active txn %lx 's first lsn \[%lu\]\[%lu\] |
| 2553 | \[%lu\]\[%lu\] Can not find an active transaction's ""information, txnid: %lx. |
| 2554 | \[%lu\]\[%lu\] Parent txn %lx ended ""before child txn %lx ends. |
| 2555 | \[%lu\]\[%lu\] Can not find an active ""transaction's information, txnid: %lx. |
| 2556 | \[%lu\]\[%lu\] Txn %lx ended before it commits. |
| 2557 | \[%lu\]\[%lu\] Can not find an active transaction's ""information, txnid: %lx. |
| 2558 | \[%lu\]\[%lu\] Multiple txn_prepare log record for ""transaction %lx, previous prepare lsn: \[%lu, %lu\]. |
| 2559 | \[%lu\]\[%lu\] \[WARNING\] This log record of type %s ""does not have a greater time stamp than ""\[%lu, %lu\] of type %s |
| 2560 | \[%lu\]\[%lu\] Transaction %lx is updating a ""db file %d not registered. |
| 2561 | \[%lu\]\[%lu\] Can not find an active transaction's ""information, txnid: %lx. |
| 2562 | \[%lu\]\[%lu\] Previous record for transaction %lx is ""\[%lu\]\[%lu\] and prev_lsn is \[%lu\]\[%lu\]. |
| 2563 | \[%lu\]\[%lu\] Update action is performed in a ""prepared transaction %lx. |
| 2564 | \[%lu\]\[%lu\] Transaction id %lx reused without ""being recycled with a \_\_txn_recycle. |
| 2565 | \[%lu\]\[%lu\] Non-transactional update, ""log type: %u, fileid: %d. |
| 2566 | \[%lu\]\[%lu\] Can not find an active transaction's ""information, txnid: %lx. |
| 2567 | \[%lu\]\[%lu\] Txn %lx aborted after this log record. |
| 2568 | The number of active, committed and aborted child txns ""of txn %lx: %u, %u, %u. |
| 2569 | log region size must be \>= %d |
| 2570 | no absolute path for the current directory |
| 2571 | log file auto-remove |
| 2572 | DB_ENV-\>log_archive: bad log record |
| 2573 | DB_ENV-\>log_archive: unable to read log record |
| 2574 | DB_LOGC-\>get: unset cursor |
| 2575 | DB_LOGC-\>get: invalid LSN: %lu/%lu |
| 2576 | Encountered zero length records while traversing backwards |
| 2577 | DB_LOGC-\>get: log record LSN %lu/%lu: ""checksum mismatch, hdr.chksum: %s, hdr.prev: %u, ""hdr.len: %u, log type: %u. Skipping it and ""continuing with the %s one |
| 2578 | DB_LOGC-\>get: log record LSN %lu/%lu: checksum mismatch |
| 2579 | DB_LOGC-\>get: catastrophic recovery may be required |
| 2580 | DB_LOGC-\>get: LSN %lu/%lu: invalid log record header |
| 2581 | DB_LOGC-\>get: LSN: %lu/%lu: read |
| 2582 | DB_LOGC-\>get: LSN: %lu/%lu: short read |
| 2583 | Log file %d not found, check log directory configuration |

## Memory Pool Messages

| Message Number | Message Text |
|----|----|
| 0703 | Cannot allocate space for path: %s |
| 0704 | Cannot open traget file: %s |
| 0712 | %s is already in a backup |
| 0714 | Releasing backup of %s for %s. |
| 3001 | %smethod not permitted when replication is configured |
| 3002 | %s: non-transactional update to a multiversion file |
| 3003 | individual cache size too large: maximum is 4GB |
| 3004 | individual cache size too large: maximum is 10TB |
| 3005 | Truncate beyond the end of file |
| 3006 | Can't get the required free size while |
| 3007 | DB_ENV-\>memp_trickle: %d: percent must be between 1 and 100 |
| 3008 | %s: dirty flag set for readonly file page |
| 3009 | %s: error releasing a read-only page |
| 3010 | %s: error getting a page for writing |
| 3011 | %s: more pages returned than retrieved |
| 3012 | %s: page %lu: unpinned page returned |
| 3013 | \_\_memp_fput: pinned buffer not found for thread %s |
| 3014 | unable to create temporary backing file |
| 3015 | %s: write failed for page %lu |
| 3016 | %s: %s failed for page %lu |
| 3017 | unable to allocate space from the buffer cache |
| 3018 | %s: unwritable page %d remaining in the cache after error %d |
| 3019 | cannot remove the last cache |
| 3020 | cannot resize to %lu cache regions: maximum is %lu |
| 3021 | %s: dirty flag set for readonly file page |
| 3022 | %s: page %lu: reference count overflow |
| 3023 | %s: file limited to %lu pages |
| 3024 | %s: file limited to %lu pages |
| 3025 | DB_MPOOLFILE-\>get: buffer data is NOT size_t aligned |
| 3026 | Unable to allocate memory for mpool region |
| 3027 | %s: unable to flush page: %lu |
| 3028 | %s: unable to flush |
| 3029 | DB_ENV-\>memp_fcreate: method not permitted when replication is configured |
| 3030 | get_fileid: file ID not set |
| 3031 | DB_MPOOLFILE-\>get_priority: unknown priority value: %d |
| 3032 | DB_MPOOLFILE-\>set_priority: unknown priority value: %d |
| 3033 | DB_MPOOLFILE-\>open: page sizes must be a power-of-2 |
| 3034 | DB_MPOOLFILE-\>open: clear length larger than page size |
| 3035 | DB_MPOOLFILE-\>open: temporary files can't be readonly |
| 3036 | DB_MPOOLFILE-\>open: DB_MULTIVERSION requires transactions |
| 3037 | %s: file size not a multiple of the pagesize |
| 3038 | %s: clear length, page size or LSN location changed |
| 3039 | Cannot open DURABLE and NOT DURABLE handles in the same file |
| 3040 | %s: close: %lu blocks left pinned |

## Replication Messages

| Message Number | Message Text |
|----|----|
| 3501 | Request for LSN \[%lu\]\[%lu\] not found |
| 3502 | Client initialization failed. Need to manually restore client |
| 3503 | rep_send_message: Unknown rep version %lu, my version %lu |
| 3504 | Operation locked out. Waiting for replication lockout to complete |
| 3509 | Operation locked out. Waiting for replication lockout to complete |
| 3510 | Waiting for %s (%lu) to complete replication lockout |
| 3511 | Waiting for %s (%lu) to complete replication lockout for %d minutes |
| 3512 | %s cannot call from Replication Manager application |
| 3513 | DB_ENV-\>rep_process_message: control argument must be specified |
| 3514 | Environment not configured as replication master or client |
| 3515 | DB_ENV-\>rep_process_message: error retrieving DBT contents |
| 3516 | unsupported old replication message version %lu, minimum version %d |
| 3517 | unexpected replication message version %lu, expected %d |
| 3518 | unsupported old replication log version %lu, minimum version %d |
| 3519 | unexpected log record version %lu, expected %d |
| 3520 | Inconsistent lease configuration |
| 3521 | DB_ENV-\>rep_process_message: unknown replication message: type %lu |
| 3522 | failed to read the log at \[%lu\]\[%lu\] |
| 3523 | transaction failed at \[%lu\]\[%lu\] |
| 3524 | collect failed at: \[%lu\]\[%lu\] |
| 3525 | Error syncing ckp \[%lu\]\[%lu\] |
| 3526 | Error processing txn \[%lu\]\[%lu\] |
| 3527 | DB_ENV-\>rep_elect: cannot call from Replication Manager application |
| 3528 | DB_ENV-\>rep_elect: must be called after DB_ENV-\>rep_set_transport |
| 3529 | DB_ENV-\>rep_elect: must be called after DB_ENV-\>rep_start |
| 3530 | DB_ENV-\>rep_elect: nsites must be zero if leases configured |
| 3531 | DB_ENV-\>rep_elect:WARNING: nvotes (%d) is sub-majority with nsites (%d) |
| 3532 | DB_ENV-\>rep_elect: nvotes (%d) is larger than nsites (%d) |
| 3533 | No electable site found: recvd %d of %d votes from %d sites |
| 3534 | Not enough votes to elect: recvd %d of %d from %d sites |
| 3535 | Application type mismatch for a replication |
| 3548 | %s cannot configure repmgr settings from base replication application |
| 3549 | %s in-memory replication must be configured before DB_ENV-\>open |
| 3550 | DB_ENV-\>rep_set_config: leases must be |
| 3551 | DB_ENV-\>rep_set_config: leases cannot be turned off |
| 3552 | DB_ENV-\>rep_start: cannot call from Replication Manager application |
| 3553 | DB_ENV-\>rep_start: must specify DB_REP_CLIENT or DB_REP_MASTER |
| 3554 | DB_ENV-\>rep_start: must be called after DB_ENV-\>rep_set_transport |
| 3555 | DB_ENV-\>rep_start: must call DB_ENV-\>rep_set_timeout for leases first |
| 3556 | DB_ENV-\>rep_start: Cannot become master during internal init |
| 3557 | rep_start: Cannot become master without being elected when using leases. |
| 3558 | rep_start: Cannot become master with outstanding lease granted. |
| 3559 | First record not found |
| 3560 | Checkpoint record at LSN \[%lu\]\[%lu\] not found |
| 3561 | Invalid checkpoint record at \[%lu\]\[%lu\] |
| 3562 | Checkpoint LSN record \[%lu\]\[%lu\] not found |
| 3563 | Attempt to get first log record failed |
| 3564 | Final log record not found |
| 3565 | DB_ENV-\>rep_set_nsites: cannot call from Replication Manager application |
| 3566 | timeout value must be \> 0 |
| 3567 | %scannot set Replication Manager timeout from base replication application |
| 3568 | %s: lease timeout must be set before DB_ENV-\>rep_start. |
| 3569 | Unknown timeout type argument to DB_ENV-\>rep_set_timeout |
| 3570 | unknown timeout type argument to DB_ENV-\>rep_get_timeout |
| 3571 | DB_ENV-\>rep_set_request: Invalid min or max values |
| 3572 | DB_ENV-\>rep_set_transport: cannot call from |
| 3573 | DB_ENV-\>rep_set_transport: no send function specified |
| 3574 | DB_ENV-\>rep_set_transport: eid must be greater than or equal to 0 |
| 3575 | DB_ENV-\>rep_set_clockskew: Zero only valid for |
| 3576 | DB_ENV-\>rep_set_clockskew: slow_clock value is |
| 3577 | DB_ENV-\>rep_set_clockskew: must be called before DB_ENV-\>rep_start |
| 3578 | DB_ENV-\>rep_flush: must be called after DB_ENV-\>rep_set_transport |
| 3579 | DB_ENV-\>rep_sync: must be called after DB_ENV-\>rep_set_transport |
| 3580 | non-replication commit token in replication env |
| 3581 | library build did not include support for replication |
| 3582 | closing socket |
| 3583 | releasing WSA event object |
| 3584 | can't create listen socket |
| 3585 | can't set REUSEADDR socket option |
| 3586 | can't bind socket to listening address |
| 3587 | listen() |
| 3588 | can't unblock listen socket |
| 3589 | unable to initialize Windows networking |
| 3590 | can't create event for listen socket |
| 3591 | can't enable event for listener |
| 3592 | can't set event bits 0x%lx |
| 3593 | EnumNetworkEvents |
| 3613 | "unexpected msg type %d in state %d |
| 3614 | select loop failed |
| 3615 | accept error |
| 3616 | can't set nonblock after accept |
| 3617 | connector thread failed |
| 3618 | set_nonblock in connnect thread |
| 3619 | illegal size for rep msg |
| 3620 | unexpected msg type %d in PARAMETERS state |
| 3621 | unexpected msg type rcvd in ready state: %d |
| 3622 | No available version between %lu and %lu |
| 3623 | Can't support confirmed version %lu |
| 3624 | handshake is missing rec part |
| 3625 | malformed V1 handshake |
| 3626 | can't set KEEPALIVE socket option |
| 3627 | bad ack msg size |
| 3628 | library build did not include support for the Replication Manager |
| 3629 | unexpected election failure |
| 3630 | pthread_attr_init in repmgr_thread_start |
| 3631 | pthread_attr_setstacksize in repmgr_thread_start |
| 3632 | can't access signal handler |
| 3633 | can't access signal handler |
| 3634 | select |
| 3635 | repmgr_start: unrecognized flags parameter value |
| 3636 | Replication Manager needs an environment with DB_THREAD |
| 3637 | A local site must be named before calling repmgr_start |
| 3638 | Could not clean up repmgr |
| 3639 | a non-zero flags value is required for initial repmgr_start() call |
| 3640 | repmgr is already started |
| 3641 | repmgr_start: nthreads parameter must be \>= %d |
| 3642 | can't configure repmgr elections from subordinate process |
| 3643 | subsequent repmgr_start() call may not specify DB_REP_ELECTION |
| 3644 | repmgr_start: nthreads parameter must be \>= 0 |
| 3645 | can't start selector thread |
| 3646 | unknown ack_policy in DB_ENV-\>repmgr_set_ack_policy |
| 3648 | repmgr_site: a host name is required |
| 3649 | repmgr_site: port out of range \[1,%u\] |
| 3650 | DB_ENV-\>repmgr_channel: must be called after DB_ENV-\>repmgr_start |
| 3651 | repmgr is stopped |
| 3652 | %d is not a valid remote EID |
| 3653 | set_nonblock channel |
| 3654 | DB_CHANNEL-\>send_request() not supported on DB_EID_BROADCAST channel |
| 3655 | No message dispatch call-back function has been configured |
| 3656 | Application failed to provide a response |
| 3657 | a response has already been sent |
| 3658 | originator does not accept multi-segment response |
| 3659 | originator's USERMEM buffer too small |
| 3660 | %s() invalid on DB_CHANNEL supplied to msg dispatch function |
| 3661 | %s: cannot call from base replication application |
| 3662 | Can't determine EID before env open |
| 3663 | Site config value not applicable to local site |
| 3665 | Unrecognized site config value |
| 3666 | A previously given local site may not be unset |
| 3667 | A (different) local site has already been set |
| 3668 | Local site cannot have HELPER or PEER attributes |
| 3669 | repmgr is not running |
| 3670 | No message dispatch call-back function has been configured |
| 3671 | Application failed to provide a response |
| 3672 | Nsites unknown before repmgr_start() |
| 3673 | rep_start |
| 3674 | A mismatching local site address has been set in the environment |
| 3675 | Not enough input bytes to fill a \_\_repmgr_connect_reject message |
| 3676 | unexpected msg type %lu in prepare_input |
| 3677 | unexpected msg type %lu in process_own_msg |
| 3678 | unexpected conn version %lu in send_handshake |
| 3679 | unexpected conn version %lu in accept_handshake |
| 3681 | invalid own buf size %lu in prepare_input |
| 3682 | invalid cur resp %lu in prepare_input |
| 3683 | unexpected connection info in record_permlsn |

## Sequences Messages

| Message Number | Message Text |
|----|----|
| 4001 | Zero length sequence key specified |
| 4002 | Sequences not supported in databases configured for duplicate data |
| 4003 | Sequence value out of range |
| 4004 | Sequence create failed |
| 4005 | Bad sequence record format |
| 4006 | Unsupported sequence version: %d |
| 4007 | Cache size must be \>= 0 |
| 4008 | Sequence value out of range |
| 4009 | Minimum sequence value must be less than maximum sequence value |
| 4010 | Bad sequence record format |
| 4011 | Sequence overflow |
| 4012 | Sequence update failed |
| 4013 | Sequence overflow |
| 4014 | Number of items to be cached is larger than the sequence range |
| 4015 | library build did not include support for sequences |
| 4016 | Heap databases may not be used with sequences. |

## Transaction Messages

| Message Number | Message Text |
|----|----|
| 4501 | Transaction has in memory logs |
| 4502 | Transaction has in memory logs |
| 4503 | Aborting txn %#lx: %s |
| 4504 | Transaction abort failed |
| 4505 | operation not permitted while in recovery |
| 4506 | Invalid checkpoint record at \[%lu\]\[%lu\] |
| 4507 | No log records |
| 4508 | Unable to allocate memory for the transaction region |
| 4509 | unable to discard txn %#lx |
| 4510 | unable to abort transaction %#lx |
| 4511 | Error: closing the transaction region with active transactions |
| 4512 | Current ID value %lu below minimum |
| 4513 | Maximum ID value %lu below minimum |
| 4514 | txnid %lx commit record found, already on commit list |
| 4515 | transaction not in list %lx |
| 4516 | Transaction not in list %x |
| 4517 | txnid %lx commit record found, already on commit list |
| 4518 | txn_checkpoint: failed to flush the buffer cache |
| 4519 | txn_checkpoint: failed to flush the buffer cache |
| 4520 | txn_checkpoint: log failed at LSN \[%ld %ld\] |
| 4521 | Family transactions cannot have parents |
| 4522 | Child transaction snapshot setting must match parent |
| 4523 | Unable to allocate transaction recycle buffer |
| 4524 | operation not permitted during recovery |
| 4525 | Unable to allocate memory for transaction detail |
| 4526 | commit token unavailable for nested txn |
| 4527 | may not be called on a replication client |
| 4528 | DB_TXN-\>prepare: log_write failed |
| 4529 | Unable to allocate memory for transaction name |
| 4530 | operation not permitted during recovery |
| 4531 | transaction has active cursors |
| 4532 | not a restored transaction |
| 4533 | Prepare disallowed on child transactions |
| 4534 | transaction already prepared |
| 4535 | transaction already %s |
| 4536 | DB_TXN-\>abort: in-memory log undo failed |
| 4537 | DB_TXN-\>abort: log undo failed for LSN: %lu %lu |
| 4538 | Child transaction is active |
| 4539 | replication commit token in non-replication env |
| 4540 | xa_get_txn: transaction begin failed |
| 4541 | xa_get_txn: XA transaction with parent |
| 4542 | xa_get_txn: transaction does not exist |
| 4543 | xa_get_txn: txn_continue fails |
| 4544 | xa_get_txn: os_malloc failed |
| 4545 | xa_open: Failure creating env handle |
| 4546 | xa_open: Failure setting thread count |
| 4547 | xa_open: Failure opening environment |
| 4548 | xa_open: Failure getting log configuration |
| 4549 | xa_open: In-memory logging not allowed in XA environment |
| 4550 | xa_start: failure mapping xid |
| 4551 | xa_end: failure mapping xid |
| 4552 | xa_end: cannot end with open cursors |
| 4553 | xa_end: txn_detail mismatch |
| 4554 | xa_end: ending transaction that is idle |
| 4555 | xa_prepare: failure mapping xid |
| 4556 | xa_prepare: xid not found |
| 4557 | xa_prepare: transaction neither active nor idle |
| 4558 | xa_prepare: txnp-\>prepare failed |
| 4559 | xa_commit: failure mapping xid |
| 4560 | xa_commit: xid not found |
| 4561 | xa_commit: commiting transaction active in branch |
| 4562 | xa_commit: attempting to commit unprepared transaction |
| 4563 | xa_commit: txnp-\>commit failed |
| 4564 | xa_recover: txn_get_prepared failed |
| 4565 | xa_rollback: failure mapping xid |
| 4566 | xa_rollback: xid not found |
| 4567 | xa_rollback: transaction in invalid state %d |
| 4568 | xa_rollback: failure aborting transaction |
| 4569 | xa_forget: failure mapping xid |
| 4570 | xa_forget: xid not found |
| 4571 | xa_forget: txnp-\>discard failed |

## Command Line Utilities Messages

| Message Number | Message Text |
|----|----|
| 5001 | %s: Unsupported database type |
| 5002 | %s: version %d.%d doesn't match library version %d.%d |
| 5003 | %s: version %d.%d doesn't match library version %d.%d |
| 5004 | \[%lu\]\[%lu\] App-specific log record: %lu data: |
| 5005 | %s: strdup: %s |
| 5006 | %s: illegal option combination |
| 5007 | Unknown statistics flag |
| 5008 | close |
| 5009 | %s: version %d.%d doesn't match library version %d.%d |
| 5010 | %s: strdup: %s |
| 5011 | callback: initialization |
| 5012 | callback: initialization |
| 5013 | tx: dispatch |
| 5014 | Unknown version %lu |
| 5015 | %s: version %d.%d doesn't match library version %d.%d |
| 5016 | \[%lu\]\[%lu\]application specific record: rec: %lu |
| 5017 | data: |
| 5018 | %s: strdup: %s |
| 5019 | %s: %s upgraded successfully |
| 5020 | %s: version %d.%d doesn't match library version %d.%d |
| 5021 | %s: strdup: %s |
| 5022 | recovery %d%% complete |
| 5023 | %s: localtime: %s |
| 5024 | %s: out of range or illegal time specification: \[\[CC\]YY\]MMDDhhmm\[.SS\] |
| 5025 | %s: version %d.%d doesn't match library version %d.%d |
| 5026 | strdup: %s |
| 5027 | cannot specify -d and -c |
| 5028 | cannot specify -D and -d or -l |
| 5029 | failed to get environment variable DB_HOME: %s |
| 5030 | no source database environment specified |
| 5031 | no target backup directory specified |
| 5032 | hot backup started at %s |
| 5033 | DB_CONFIG must not contain an absolute |
| 5035 | %s: force checkpoint |
| 5036 | %s: remove unnecessary log files |
| 5040 | %s: run catastrophic recovery |
| 5041 | %s: remove unnecessary log files |
| 5042 | hot backup completed at %s |
| 5043 | HOT BACKUP FAILED! |
| 5057 | %s: cannot specify -l with conflicting DB_CONFIG file |
| 5058 | use of -l with DB_CONFIG file is deprecated |
| 5071 | version %d.%d doesn't match library version %d.%d |
| 5072 | %s: %s: reopen: %s |
| 5073 | %s: strdup: %s |
| 5074 | No keys specified in file |
| 5075 | Keys specified in file |
| 5076 | Btree and Hash must specify keys |
| 5077 | improper database type conversion specified |
| 5078 | no database type specified |
| 5079 | odd number of key/data pairs |
| 5080 | %s: line %d: key already exists, not loaded: |
| 5081 | command-line configuration uses name=value format |
| 5082 | unknown command-line configuration keyword "%s" |
| 5083 | error reading db name |
| 5084 | line %lu: VERSION %d is unsupported |
| 5085 | line %lu: unknown type |
| 5086 | unknown input-file header configuration keyword "%s" |
| 5087 | line %lu: unexpected format |
| 5088 | unable to allocate memory |
| 5089 | boolean name=value pairs require a value of 0 or 1 |
| 5090 | unexpected end of input data or key/data pair |
| 5091 | %s: version %d.%d doesn't match library version %d.%d |
| 5092 | Cannot run %s without Replication Manager. |
| 5093 | Program name too long |
| 5094 | %s: strdup: %s |
| 5095 | db_replicate begin: %s |
| 5096 | received panic event |
| 5097 | ignoring event %d |
| 5098 | %s: version %d.%d doesn't match library version %d.%d |
| 5099 | %s: %lu %s %s: message too long |
| 5100 | %s: strdup: %s |
| 5101 | open |
| 5102 | running at %.24s |
| 5103 | rejected %d locks |
| 5104 | %s: version %d.%d doesn't match library version %d.%d |
| 5105 | Verification of %s %s. |
| 5106 | close |
| 5107 | %s: version %d.%d doesn't match library version %d.%d |
| 5108 | %s: %s: reopen: %s |
| 5109 | %s: strdup: %s |
| 5110 | %s: the -d and -p options may not both be specified |
| 5111 | %s: the -l and -s options may not both be specified |
| 5112 | %s: the -m option may not be specified with -l or -s |
| 5113 | %s: the -k and -r or -R options may not both be specified |
| 5114 | %s: the -r or R options may not be specified with -s |
| 5115 | open: %s |
| 5116 | %s: does not contain multiple databases |
| 5117 | close |
| 5118 | %s: version %d.%d doesn't match library version %d.%d |
| 5119 | %s: strdup: %s |
| 5120 | %s: version %d.%d doesn't match library version %d.%d |
| 5121 | %s: strdup: %s |
| 5122 | %s: at least one of -1, -k and -p must be specified |
| 5123 | checkpoint begin: %s |
| 5124 | checkpoint complete: %s |
| 5125 | %s: version %d.%d doesn't match library version %d.%d |
| 5126 | dbm: no open database. |
| 5127 | Heap must not specify keys |
| 5128 | improper database type conversion specified |
| 5129 | Cannot copy data from a PRIVATE environment |
| 5130 | Password may not be specified twice |
| 5131 | Password may not be specified twice |
| 5132 | Password may not be specified twice |
| 5133 | Password may not be specified twice |
| 5134 | Password may not be specified twice |
| 5135 | Password may not be specified twice |
| 5136 | Password may not be specified twice |
| 5137 | Password may not be specified twice |
| 5138 | Password may not be specified twice |
| 5139 | Password may not be specified twice |
