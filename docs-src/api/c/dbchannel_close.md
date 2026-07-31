---
title: "DB_CHANNEL->close()"
api-name: "DB_CHANNEL->close()"
source: docs/api_reference/C/dbchannel_close.html
---
## DB_CHANNEL-\>close()

``` c
#include <db.h>

int
DB_CHANNEL->close(DB_CHANNEL *channel, u_int32_t flags);  
```

The `DB_CHANNEL->close()` method closes the DB_CHANNEL handle, freeing any resources allocated to the handle. All DB_CHANNEL handles must be closed before the encompassing environment handle is closed. Also, all on-going messaging operations on the channel should be allowed to complete before attempting to close the channel handle.

After `DB_CHANNEL->close()` has been called, regardless of its return, the DB_CHANNEL handle may not be accessed again.

The `DB_CHANNEL->close()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### flags

This parameter is currently unused, and must be set to 0.

### Errors

The `DB_CHANNEL->close()` method may fail and return one of the following non-zero errors:

#### EINVAL

If this method is called from a Base API application, or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
