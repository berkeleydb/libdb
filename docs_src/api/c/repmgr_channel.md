---
title: "DB_ENV->repmgr_channel()"
api-name: "DB_ENV->repmgr_channel()"
source: docs/api_reference/C/repmgr_channel.html
---
## DB_ENV-\>repmgr_channel()

``` c
#include <db.h>

int
DB_ENV->repmgr_channel(DB_ENV *env, int eid, DB_CHANNEL **channelp, 
    u_int32_t flags);  
```

The `DB_ENV->repmgr_channel()` method returns a DB_CHANNEL handle. This is used to create and manage custom message traffic between the sites in the replication group.

This method allocates memory for the handle, returning a pointer to the structure in the memory to which **channelp** refers. To release the allocated memory and discard the handle, call the <a href="dbchannel_close.md" class="xref" title="DB_CHANNEL-&gt;close()">DB_CHANNEL-&gt;close()</a> method.

The `DB_ENV->repmgr_channel()` method may be called at any time after <a href="repmgrstart.md" class="xref" title="DB_ENV-&gt;repmgr_start()">DB_ENV-&gt;repmgr_start()</a> has been called with a `0` return code.

The `DB_ENV->repmgr_channel()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### eid

This parameter must be set to one of the following:

- The numerical env ID of a remote site in the replication group.

- `DB_EID_MASTER`

  Messages sent on this channel are sent only to the master site. Note that messages are always sent to the current master, even if the master has changed since the channel was opened. If the current master is disconnected or unknown, the operation fails and repmgr returns an error code.

  If the local site is the master, then sending messages on this channel will result in the local site receiving those messages echoed back to itself.

#### channelp

References memory into which a pointer to the allocated handle is copied.

#### flags

This parameter is currently unused, and must be set to 0.

### Errors

The `DB_ENV->repmgr_channel()` method may fail and return one of the following non-zero errors:

#### EINVAL

If this method is called from a Base API application; or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
