---
title: "New Hotbackup Interface"
api-name: "New Hotbackup Interface"
source: docs/installation/upgrade_11gr2_53_hotbackup.html
---
## New Hotbackup Interface

<span class="sect2"> [New Functions](upgrade_11gr2_53_hotbackup.md#idp815256) </span>

<span class="sect2"> [Flags Accepted by DB_ENV-\>backup()](upgrade_11gr2_53_hotbackup.md#idp805032) </span>

<span class="sect2"> [Flags Accepted by DB_ENV-\>dbbackup()](upgrade_11gr2_53_hotbackup.md#idp822632) </span>

<span class="sect2"> [Enumerations Accepted by DB_ENV-\>set_backup_config()](upgrade_11gr2_53_hotbackup.md#idp828456) </span>

Two new functions have been added to the API that perform hotbackups, <a href="../../api/c/envbackup.md" class="olink">DB_ENV-&gt;backup()</a> and <a href="../../api/c/envdbbackup.md" class="olink">DB_ENV-&gt;dbbackup()</a>. <a href="../../api/c/envbackup.md" class="olink">DB_ENV-&gt;backup()</a> creates a hotbackup of all databases in the specified environment, and <a href="../../api/c/envdbbackup.md" class="olink">DB_ENV-&gt;dbbackup()</a> creates a hotbackup of the specified database. The functions <a href="../../api/c/envset_backup_callbacks.md" class="olink">DB_ENV-&gt;set_backup_callbacks()</a> and <a href="../../api/c/envset_backup_config.md" class="olink">DB_ENV-&gt;set_backup_config()</a> can be called to customize the behavior of hotbackup. Note that this interface must be used to create a hotbackup on all non-BSD or Unix based systems.

### New Functions

- <a href="../../api/c/envbackup.md" class="olink">DB_ENV-&gt;backup()</a>
- <a href="../../api/c/envdbbackup.md" class="olink">DB_ENV-&gt;dbbackup()</a>
- <a href="../../api/c/envset_backup_callbacks.md" class="olink">DB_ENV-&gt;set_backup_callbacks()</a>
- <a href="../../api/c/envset_backup_config.md" class="olink">DB_ENV-&gt;set_backup_config()</a>

### Flags Accepted by <a href="../../api/c/envbackup.md" class="olink">DB_ENV-&gt;backup()</a>

- `DB_BACKUP_CLEAN`
- `DB_BACKUP_FILES`
- `DB_BACKUP_NO_LOGS`
- `DB_BACKUP_SINGLE_DIR`
- `DB_BACKUP_UPDATE`
- `DB_CREATE`
- `DB_EXCL`
- `DB_VERB_BACKUP`

### Flags Accepted by <a href="../../api/c/envdbbackup.md" class="olink">DB_ENV-&gt;dbbackup()</a>

- `DB_CREATE`
- `DB_EXCL`

### Enumerations Accepted by <a href="../../api/c/envset_backup_config.md" class="olink">DB_ENV-&gt;set_backup_config()</a>

- `DB_BACKUP_WRITE_DIRECT`
- `DB_BACKUP_READ_COUNT`
- `DB_BACKUP_READ_SLEEP`
- `DB_BACKUP_SIZE`
