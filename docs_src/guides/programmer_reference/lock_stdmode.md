---
title: "Standard lock modes"
api-name: "Standard lock modes"
source: docs/programmer_reference/lock_stdmode.html
---
## Standard lock modes

The Berkeley DB locking protocol is described by a conflict matrix. A conflict matrix is an NxN array in which N is the number of different lock modes supported, and the (i, j)th entry of the array indicates whether a lock of mode i conflicts with a lock of mode j. In addition, Berkeley DB defines the type **db_lockmode_t**, which is the type of a lock mode within a conflict matrix.

The following is an example of a conflict matrix. The actual conflict matrix used by Berkeley DB to support the underlying access methods is more complicated, but this matrix shows the lock mode relationships available to applications using the Berkeley DB Locking subsystem interfaces directly.

<span class="term">DB_LOCK_NG</span>  
not granted (always 0)

<span class="term">DB_LOCK_READ</span>  
read (shared)

<span class="term">DB_LOCK_WRITE</span>  
write (exclusive)

<span class="term">DB_LOCK_IWRITE</span>  
intention to write (shared)

<span class="term">DB_LOCK_IREAD</span>  
intention to read (shared)

<span class="term">DB_LOCK_IWR</span>  
intention to read and write (shared)

In a conflict matrix, the rows indicate the lock that is held, and the columns indicate the lock that is requested. A 1 represents a conflict (that is, do not grant the lock if the indicated lock is held), and a 0 indicates that it is OK to grant the lock.

``` c
               Notheld    Read    Write    IWrite    IRead    IRW
Notheld           0         0       0         0         0      0
Read*             0         0       1         1         0      1
Write**           0         1       1         1         1      1
Intent Write      0         1       1         0         0      0
Intent Read       0         0       1         0         0      0
Intent RW         0         1       1         0         0      0
```

<span class="term">\*</span>  
In this case, suppose that there is a read lock held on an object. A new request for a read lock would be granted, but a request for a write lock would not.

<span class="term">\*\*</span>  
In this case, suppose that there is a write lock held on an object. A new request for either a read or write lock would be denied.
