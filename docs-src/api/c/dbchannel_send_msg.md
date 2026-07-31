---
title: "DB_CHANNEL->send_msg()"
api-name: "DB_CHANNEL->send_msg()"
source: docs/api_reference/C/dbchannel_send_msg.html
---
## DB_CHANNEL-\>send_msg()

``` c
#include <db.h>

int
DB_CHANNEL->send_msg(DB_CHANNEL *channel, DBT *msg, u_int32_t nmsg,
                     u_int32_t flags);  
```

The `DB_CHANNEL->send_msg()` method sends a message on the message channel. The message is sent asynchronously; the method does not wait for a response before returning. This method usually completes quickly because it only waits for the local TCP implementation to accept the bytes into its network data buffer. However, this message could block briefly for longer messages, and/or if the network data buffer is nearly full. This method could even block indefinitely if the remote site is slow to read.

If you want to block while waiting for a response from a remote site, use the <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> method instead of this method.

The message sent by this method is received and handled at remote sites using a message dispatch callback, which is configured using the <a href="repmgr_msg_dispatch.md" class="xref" title="DB_ENV-&gt;repmgr_msg_dispatch()">DB_ENV-&gt;repmgr_msg_dispatch()</a> method. Note that the DB_CHANNEL-\>send_msg() method may be used within the the message dispatch callback on the remote site to send a response or acknowledgement for messages that it receives and is handling.

This method may be used on channels opened to any destination (see the <a href="repmgr_channel.md" class="xref" title="DB_ENV-&gt;repmgr_channel()">DB_ENV-&gt;repmgr_channel()</a> method for a list of potential destinations).

The `DB_CHANNEL->send_msg()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### msg

Refers to an array of DBT handles. For more information, see <a href="dbt.md" class="xref" title="Chapter 4.  The DBT Handle">The DBT Handle</a> .

Any flags provided to the DBT handles used in this array are ignored.

#### nmsg

Indicates how many elements are contained in the `msg` array.

#### flags

This parameter is currently unused, and must be set to 0.

### Errors

The `DB_CHANNEL->send_msg()` method may fail and return one of the following non-zero errors:

#### DB_NOSERVER

A message was sent to a remote site that has not configured a message dispatch call-back function. Use the <a href="repmgr_msg_dispatch.md" class="xref" title="DB_ENV-&gt;repmgr_msg_dispatch()">DB_ENV-&gt;repmgr_msg_dispatch()</a> method at every site belonging to the replication group to configure a message dispatch call-back function.

#### EINVAL

If this method is called from a Base API application, or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
