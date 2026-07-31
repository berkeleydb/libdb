---
title: "DB_ENV->set_verbose"
api-name: "DB_ENV->set_verbose"
source: docs/upgrading/upgrade_4_3_verb.html
---
## DB_ENV-\>set_verbose

The 4.3 release removes support for the <a href="../../api/c/envset_verbose.md" class="olink">DB_ENV-&gt;set_verbose()</a> method flag DB_VERB_CHKPOINT. Application writers should simply remove any use of this flag from their applications.

The 4.3 release redirects output configured by the <a href="../../api/c/envset_verbose.md" class="olink">DB_ENV-&gt;set_verbose()</a> method from the error output channels (see the <a href="../../api/c/envset_errfile.md" class="olink">DB_ENV-&gt;set_errfile()</a> and <a href="../../api/c/envset_errcall.md" class="olink">DB_ENV-&gt;set_errcall()</a> methods for more information) to the new <a href="../../api/c/envset_msgcall.md" class="olink">DB_ENV-&gt;set_msgcall()</a> and <a href="../../api/c/envset_msgfile.md" class="olink">DB_ENV-&gt;set_msgfile()</a> message output channels. This change means the error output channels are now only used for errors, and not for debugging and performance tuning messages as well as errors. Application writers using <a href="../../api/c/envset_verbose.md" class="olink">DB_ENV-&gt;set_verbose()</a> should confirm that output is handled appropriately.
