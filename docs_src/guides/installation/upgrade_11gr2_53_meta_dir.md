---
title: "Configure Directory to Store Metadata Files"
api-name: "Configure Directory to Store Metadata Files"
source: docs/installation/upgrade_11gr2_53_meta_dir.html
---
## Configure Directory to Store Metadata Files

<span class="sect2"> [New Functions](upgrade_11gr2_53_meta_dir.md#idp837576) </span>

The directory in which persistent metadata files are stored can now be configured. By default persistent metadata files are stored in the environment home directory. The files that will be stored in the metadata directory are \_\_db.rep.system, \_\_db.rep.gen, \_\_db.rep.egen and \_\_db.rep.init.

To set the metadata file directory, call <a href="../../api/c/envset_metadata_dir.md" class="olink">DB_ENV-&gt;set_metadata_dir()</a> with the path to the directory in which to store metadata files. The metadata directory can also be set in the `DB_CONFIG` file using `set_metadata_dir`.

### New Functions

- <a href="../../api/c/envset_metadata_dir.md" class="olink">DB_ENV-&gt;set_metadata_dir()</a>
- <a href="../../api/c/envget_metadata_dir.md" class="olink">DB_ENV-&gt;get_metadata_dir()</a>
