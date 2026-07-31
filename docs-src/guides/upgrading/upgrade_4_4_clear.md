---
title: "DB_MPOOLFILE->set_clear_len"
api-name: "DB_MPOOLFILE->set_clear_len"
source: docs/upgrading/upgrade_4_4_clear.html
---
## DB_MPOOLFILE-\>set_clear_len

The meaning of a 0 "clear length" argument to the <a href="../../api/c/mempset_clear_len.md" class="olink">DB_MPOOLFILE-&gt;set_clear_len()</a> method changed in the Berkeley DB 4.4 release. In previous releases, specifying a length of 0 was equivalent to the default, and the entire created page was cleared. Unfortunately, this left no way to specify that no part of the page needed to be cleared. In the 4.4 release, specifying a "clear length" argument of 0 means that no part of the page need be cleared.

Applications specifying a 0 "clear length" argument to the <a href="../../api/c/mempset_clear_len.md" class="olink">DB_MPOOLFILE-&gt;set_clear_len()</a> method should simply remove the call, as the default behavior is to clear the entire created page.
