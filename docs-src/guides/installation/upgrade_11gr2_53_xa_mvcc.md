---
title: "Berkeley DB X/Open Compliant XA Resource Manager and Transaction Snapshots"
api-name: "Berkeley DB X/Open Compliant XA Resource Manager and Transaction Snapshots"
source: docs/installation/upgrade_11gr2_53_xa_mvcc.html
---
## Berkeley DB X/Open Compliant XA Resource Manager and Transaction Snapshots

The transactions managed by the Berkeley DB X/open compliant XA resource manager can now be enabled for transaction snapshots. To enable snapshots open an XA managed database with the flag, `DB_MULTIVERSION`, and all XA managed transactions that operate on that database will use transaction snapshots.
