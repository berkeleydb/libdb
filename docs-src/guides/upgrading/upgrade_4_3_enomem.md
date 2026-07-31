---
title: "ENOMEM and DbMemoryException"
api-name: "ENOMEM and DbMemoryException"
source: docs/upgrading/upgrade_4_3_enomem.html
---
## ENOMEM and DbMemoryException

In versions of Berkeley DB before 4.3, the error **ENOMEM** was used to indicate that the buffer in a <a href="../../api/c/dbt.md" class="olink">DBT</a> configured with <a href="../../api/c/dbt.md#dbt_DB_DBT_USERMEM" class="olink">DB_DBT_USERMEM</a> was too small to hold a key or data item being retrieved. The 4.3 release adds a new error, `DB_BUFFER_SMALL`, that is returned in this case.

The reason for the change is that the use of **ENOMEM** was ambiguous: calls such as <a href="../../api/c/dbget.md" class="olink">DB-&gt;get()</a> or <a href="../../api/c/dbcget.md" class="olink">DBC-&gt;get()</a> could return **ENOMEM** either if a <a href="../../api/c/dbt.md" class="olink">DBT</a> was too small or if some resource was exhausted.

The result is that starting with the 4.3 release, C applications should always treat **ENOMEM** as a fatal error. Code that checked for the **ENOMEM** return and allocated a new buffer should be changed to check for `DB_BUFFER_SMALL`.

In C++ applications configured for exceptions, a <a href="../api_reference/CXX/dbmemory.html" class="olink">DbMemoryException</a> will continue to be thrown in both cases, and applications should check the errno in the exception to determine which error occurred.

In Java applications, a **DbMemoryException** will be thrown when a **Dbt** is too small to hold a return value, and an **OutOfMemoryError** will be thrown in all cases of resource exhaustion.
