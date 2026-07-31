---
title: "memp_stat"
api-name: "memp_stat"
source: docs/upgrading/upgrade_3_0_memp_stat.html
---
## memp_stat

The **st_refcnt** field returned from the memp_stat function has been removed, and this information is no longer available.

The **st_cachesize** field returned from the memp_stat function has been replaced with two new fields, **st_gbytes** and **st_bytes**.
