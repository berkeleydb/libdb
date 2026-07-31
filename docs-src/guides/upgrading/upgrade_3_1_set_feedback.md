---
title: "DB_ENV->set_feedback, DB->set_feedback"
api-name: "DB_ENV->set_feedback, DB->set_feedback"
source: docs/upgrading/upgrade_3_1_set_feedback.html
---
## DB_ENV-\>set_feedback, DB-\>set_feedback

Starting with the 3.1 release of Berkeley DB, the <a href="../../api/c/envset_feedback.md" class="olink">DB_ENV-&gt;set_feedback()</a> and <a href="../../api/c/dbset_feedback.md" class="olink">DB-&gt;set_feedback()</a> methods may return an error value, that is, they are no longer declared as returning no value, instead they return an int or throw an exception as appropriate when an error occurs.

If your application calls these functions, you may want to check for a possible error on return.
