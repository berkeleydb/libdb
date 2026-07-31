---
title: "DB_CHANNEL->set_timeout()"
api-name: "DB_CHANNEL->set_timeout()"
source: docs/api_reference/C/dbchannel_set_timeout.html
---
## DB_CHANNEL-\>set_timeout()

``` c
#include <db.h>

int
DB_CHANNEL->set_timeout(DB_CHANNEL *channel, db_timeout_t timeout); 
```

The `DB_CHANNEL->set_timeout()` method sets the default timeout value for the DB_CHANNEL handle. This timeout is used by the <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> method.

The `DB_CHANNEL->set_timeout()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### timeout

Configures the amount of time that may elapse while the <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> method waits for a message response. The timeout value must be specified as an unsigned 32-bit number of microseconds, limiting the maximum timeout to roughly 71 minutes.

### Errors

The `DB_CHANNEL->set_timeout()` method may fail and return one of the following non-zero errors:

#### EINVAL

If this method is called from a Base API application, or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
