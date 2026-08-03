---
title: "Unsupported PRAGMAs"
api-name: "Unsupported PRAGMAs"
source: docs/bdb-sql/unsupportedpragmas.html
---
## Unsupported PRAGMAs

The following PRAGMAs are not supported by the BDB SQL interface.

|  |
|----|
| <a href="http://www.sqlite.org/pragma.html#pragma_journal_mode" class="ulink" target="_top">PRAGMA journal_mode</a> |
| <a href="http://www.sqlite.org/pragma.html#pragma_legacy_file_format" class="ulink" target="_top">PRAGMA legacy_file_format</a> |

Also, <a href="http://www.sqlite.org/pragma.html#pragma_fullfsync" class="ulink" target="_top">PRAGMA fullfsync</a> is always on for the BDB SQL interface. (This is an issue only for Mac OS X platforms.)
