---
title: "SCO"
api-name: "SCO"
source: docs/installation/build_unix_sco.html
---
## SCO

1.  **If I build with gcc, programs such as db_dump and db_stat core dump immediately when invoked.**

    We suspect gcc or the runtime loader may have a bug, but we haven't tracked it down. If you want to use gcc, we suggest building static libraries.
