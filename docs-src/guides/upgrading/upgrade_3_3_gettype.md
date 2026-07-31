---
title: "DB->get_type"
api-name: "DB->get_type"
source: docs/upgrading/upgrade_3_3_gettype.html
---
## DB-\>get_type

The <a href="../../api/c/dbget_type.md" class="olink">DB-&gt;get_type()</a> method method can return an error in the Berkeley DB 3.3 release, and so requires an interface change. C and C++ applications calling <a href="../../api/c/dbget_type.md" class="olink">DB-&gt;get_type()</a> should be changed to treat the method's return as an error code, and to pass an additional second argument of type **DBTYPE \*** to the method. The additional argument is used as a memory location in which to store the requested information.
