---
title: "Encryption"
api-name: "Encryption"
source: docs/programmer_reference/env_encrypt.html
---
## Encryption

Berkeley DB optionally supports encryption using the Rijndael/AES (also known as the Advanced Encryption Standard and Federal Information Processing Standard (FIPS) 197) algorithm for encryption or decryption. The algorithm is configured to use a 128-bit key. Berkeley DB uses a 16-byte initialization vector generated using the Mersenne Twister. All encrypted information is additionally checksummed using the SHA1 Secure Hash Algorithm, using a 160-bit message digest.

The encryption support provided with Berkeley DB is intended to protect applications from an attacker obtaining physical access to the media on which a Berkeley DB database is stored, or an attacker compromising a system on which Berkeley DB is running but who is unable to read system or process memory on that system. **The encryption support provided with Berkeley DB will not protect applications from attackers able to read system memory on the system where Berkeley DB is running.**

To encrypt a database, you must configure the database for encryption prior to creating it. If you are using a database environment, you must also configure the environment for encryption. In order to create an encrypted database within an environment, you:

1.  Configure the environment for encryption using the <a href="../../api/c/envset_encrypt.md" class="olink">DB_ENV-&gt;set_encrypt()</a> method.

2.  Open the database environment.

3.  Specify the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_ENCRYPT" class="olink">DB_ENCRYPT</a> flag to the database handle.

4.  Open the database.

Once you have done that, all of the databases that you create in the environment are encrypted/decrypted by the password you specify using the <a href="../../api/c/envset_encrypt.md" class="olink">DB_ENV-&gt;set_encrypt()</a> method.

For databases not created in an environment:

1.  Specify the <a href="../../api/c/dbset_flags.md#dbset_flags_DB_ENCRYPT" class="olink">DB_ENCRYPT</a> flag to the database handle.

2.  Call the <a href="../../api/c/dbset_encrypt.md" class="olink">DB-&gt;set_encrypt()</a> method.

3.  Open the database.

Note that databases cannot be converted to an encrypted format after they have been created without dumping and re-creating them. Finally, encrypted databases cannot be read on systems with a different endianness than the system that created the encrypted database.

Each encrypted database environment (including all its encrypted databases) is encrypted using a single password and a single algorithm. Applications wanting to provide a finer granularity of database access must either use multiple database environments or implement additional access controls outside of Berkeley DB.

The only encrypted parts of a database environment are its databases and its log files. Specifically, the <a href="env_region.md" class="xref" title="Shared memory regions">Shared memory regions</a> supporting the database environment are not encrypted. For this reason, it may be possible for an attacker to read some or all of an encrypted database by reading the on-disk files that back these shared memory regions. To prevent such attacks, applications may want to use in-memory filesystem support (on systems that support it), or the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> or <a href="../../api/c/envopen.md#envopen_DB_SYSTEM_MEM" class="olink">DB_SYSTEM_MEM</a> flags to the <a href="../../api/c/envopen.md" class="olink">DB_ENV-&gt;open()</a> method, to place the shared memory regions in memory that is never written to a disk. As some systems page system memory to a backing disk, it is important to consider the specific operating system running on the machine as well. Finally, when backing database environment shared regions with the filesystem, Berkeley DB can be configured to overwrite the shared regions before removing them by specifying the <a href="../../api/c/envset_flags.md#set_flags_DB_OVERWRITE" class="olink">DB_OVERWRITE</a> flag. This option is only effective in the presence of fixed-block filesystems, journaling or logging filesystems will require operating system support and probably modification of the Berkeley DB sources.

While all user data is encrypted, parts of the databases and log files in an encrypted environment are maintained in an unencrypted state. Specifically, log record headers are not encrypted, only the actual log records. Additionally, database internal page header fields are not encrypted. These page header fields includes information such as the page's <a href="../../api/c/lsn.md" class="olink">DB_LSN</a> number and position in the database's sort order.

Log records distributed by a replication master to replicated clients are transmitted to the clients in unencrypted form. If encryption is desired in a replicated application, the use of a secure transport is strongly suggested.

We gratefully acknowledge:

- Vincent Rijmen, Antoon Bosselaers and Paulo Barreto for writing the Rijndael/AES code used in Berkeley DB.
- Steve Reid and James H. Brown for writing the SHA1 checksum code used in Berkeley DB.
- Makoto Matsumoto and Takuji Nishimura for writing the Mersenne Twister code used in Berkeley DB.
- Adam Stubblefield for integrating the Rijndael/AES, SHA1 checksum and Mersenne Twister code into Berkeley DB.

Berkeley DB 11g Release 2 supports encryption using Intel's Performance Primitive (IPP) on Linux. This works only on Intel processors. To use Berkeley DB with IPP encryption, you must have IPP installed along with the cryptography extension. The IPP performance is higher in most cases compared to the current AES implementation. See <a href="../../guides/installation/build_unix_conf.md#build_unix_conf.--with-cryptography" class="olink">--with-cryptography</a> for more information. See the <a href="http://software.intel.com/en-us/articles/intel-integrated-performance-primitives-documentation/" class="ulink" target="_top">Intel Documenation</a> for more information on IPP.
