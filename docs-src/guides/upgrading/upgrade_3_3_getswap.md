---
title: "DB->get_byteswapped"
api-name: "DB->get_byteswapped"
source: docs/upgrading/upgrade_3_3_getswap.html
---
## DB-\>get_byteswapped

The <a href="../../api/c/dbget_byteswapped.md" class="olink">DB-&gt;get_byteswapped()</a> method method can return an error in the Berkeley DB 3.3 release, and so requires an interface change. C and C++ applications calling <a href="../../api/c/dbget_byteswapped.md" class="olink">DB-&gt;get_byteswapped()</a> should be changed to treat the method's return as an error code, and to pass an additional second argument of type **int \*** to the method. The additional argument is used as a memory location in which to store the requested information.
