---
title: "DB->verify"
api-name: "DB->verify"
source: docs/upgrading/upgrade_4_2_verify.html
---
## DB-\>verify

In previous releases, applications calling the <a href="../../api/c/dbverify.md" class="olink">DB-&gt;verify()</a> method had to explicitly discard the <a href="../../api/c/db.md" class="olink">DB</a> handle by calling the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> method. Further, using the <a href="../../api/c/db.md" class="olink">DB</a> handle in other ways after calling the <a href="../../api/c/dbverify.md" class="olink">DB-&gt;verify()</a> method was not prohibited by the documentation, although such use was likely to lead to problems.

For consistency with other Berkeley DB methods, <a href="../../api/c/dbverify.md" class="olink">DB-&gt;verify()</a> method has been documented in the current release as a <a href="../../api/c/db.md" class="olink">DB</a> handle destructor. Applications using the <a href="../../api/c/db.md" class="olink">DB</a> handle in any way (including calling the <a href="../../api/c/dbclose.md" class="olink">DB-&gt;close()</a> method) after calling <a href="../../api/c/dbverify.md" class="olink">DB-&gt;verify()</a> should be updated to make no further use of any kind of the <a href="../../api/c/db.md" class="olink">DB</a> handle after <a href="../../api/c/dbverify.md" class="olink">DB-&gt;verify()</a> returns.
