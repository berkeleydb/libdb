---
title: "DB_MPOOLFILE->put"
api-name: "DB_MPOOLFILE->put"
source: docs/upgrading/upgrade_4_6_memp_fput.html
---
## DB_MPOOLFILE-\>put

The <a href="../../api/c/mempput.md" class="olink">DB_MPOOLFILE-&gt;put()</a> method takes a new parameter in the Berkeley DB 4.6 release, a page priority. This parameter allows applications to specify the page's priority when returning the page to the cache.

Applications calling the <a href="../../api/c/mempput.md" class="olink">DB_MPOOLFILE-&gt;put()</a> method can upgrade by adding a <a href="../../api/c/mempput.md#fput_DB_PRIORITY_UNCHANGED" class="olink">DB_PRIORITY_UNCHANGED</a> parameter to their calls to the <a href="../../api/c/mempput.md" class="olink">DB_MPOOLFILE-&gt;put()</a> method. This will result in no change in the application's behavior.
