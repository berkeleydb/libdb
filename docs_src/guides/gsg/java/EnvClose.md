---
title: "Closing Database Environments"
api-name: "Closing Database Environments"
source: docs/gsg/JAVA/EnvClose.html
---
## Closing Database Environments

You close your environment by calling the `Environment.close()` method. This method performs a checkpoint, so it is not necessary to perform a sync or a checkpoint explicitly before calling it. For information on checkpoints, see the *Berkeley DB, Java Edition Getting Started with Transaction Processing* guide. For information on syncs, see the *Getting Started with Transaction Processing for Java* guide.

``` c
import com.sleepycat.db.DatabaseException;

import com.sleepycat.db.Environment;

...

try {
    if (myDbEnvironment != null) {
        myDbEnvironment.close();
    } 
} catch (DatabaseException dbe) {
    // Exception handling goes here
} 
```

You should close your environment(s) only after all other database activities have completed. It is recommended that you close any databases currently open in the environment prior to closing the environment.

Closing the last environment handle in your application causes all internal data structures to be released. If there are any opened databases or stores, then DB will complain before closing them as well. At this time, any open cursors are also closed, and any on-going transactions are aborted. However, it is recommended that you always close all cursor handles immediately after their use to ensure concurrency and to release resources such as page locks.
