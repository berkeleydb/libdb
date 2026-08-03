---
title: "Database limits"
api-name: "Database limits"
source: docs/programmer_reference/am_misc_dbsizes.html
---
## Database limits

The largest database file that Berkeley DB can handle depends on the page size selected by the application. Berkeley DB stores database file page numbers as unsigned 32-bit numbers and database file page sizes as unsigned 16-bit numbers. Using the maximum database page size of 65536, this results in a maximum database file size of 2<sup>48</sup> (256 terabytes). The minimum database page size is 512 bytes, which results in a minimum maximum database size of 2<sup>41</sup> (2 terabytes).

The largest database file Berkeley DB can support is potentially further limited if the host system does not have filesystem support for files larger than 2<sup>32</sup>, including the ability to seek to absolute offsets within those files.

The largest key or data item that Berkeley DB can support is 2<sup>32</sup>, or more likely limited by available memory. Specifically, while key and data byte strings may be of essentially unlimited length, any one of them must fit into available memory so that it can be returned to the application. As some of the Berkeley DB interfaces return both key and data items to the application, those interfaces will require that any key/data pair fit simultaneously into memory. Further, as the access methods may need to compare key and data items with other key and data items, it may be a requirement that any two key or two data items fit into available memory. Finally, when writing applications supporting transactions, it may be necessary to have an additional copy of any data item in memory for logging purposes.

The maximum Btree depth is 255.
