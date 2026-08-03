---
title: "DB_CHANNEL->send_request()"
api-name: "DB_CHANNEL->send_request()"
source: docs/api_reference/C/dbchannel_send_request.html
---
## DB_CHANNEL-\>send_request()

``` c
#include <db.h>

int
DB_CHANNEL->send_request(DB_CHANNEL *channel, DBT *request, 
                     u_int32_t nrequest, DBT *response,
                     db_timeout_t timeout, u_int32_t flags);  
```

The `DB_CHANNEL->send_request()` method sends a message on the message channel. The message is sent synchronously; the method blocks waiting for a response before returning. If a response is not received within the timeout value configured for this request, this method returns with an error condition.

If you do not want to block while waiting for a response from a remote site, use the <a href="dbchannel_send_msg.md" class="xref" title="DB_CHANNEL-&gt;send_msg()">DB_CHANNEL-&gt;send_msg()</a> method.

The message sent by this method is received and handled at remote sites using a message dispatch callback, which is configured using the <a href="repmgr_msg_dispatch.md" class="xref" title="DB_ENV-&gt;repmgr_msg_dispatch()">DB_ENV-&gt;repmgr_msg_dispatch()</a> method.

The `DB_CHANNEL->send_request()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### request

Refers to an array of DBT handles. For more information, see <a href="dbt.md" class="xref" title="Chapter 4.  The DBT Handle">The DBT Handle</a> .

Any flags provided to the DBT handles used in this array are ignored.

#### nrequest

Indicates how many elements are contained in the `msg` array.

#### response

Points to a single DBT handle, which is used to receive the response from the remote site. By default, the response is expected to be a single-part message. If there is a possibility that the response could be a multi-part message, specify `DB_MULTIPLE` to this method's **flags** parameter.

The response DBT should specify one of the following flags: `DB_DBT_MALLOC`, `DB_DBT_REALLOC`, or `DB_DBT_USERMEM`.

For more information on configuring and using DBTs, see <a href="dbt.md" class="xref" title="Chapter 4.  The DBT Handle">The DBT Handle</a> .

Note that the response DBT can be empty. In this way an application can send an acknowledgement even if there is no other information that needs to be sent.

#### timeout

Configures the amount of time that may elapse while this method waits for a response from the remote site. If this timeout period elapses without a response, this method returns with an error condition.

The timeout value must be specified as an unsigned 32-bit number of microseconds, limiting the maximum timeout to roughly 71 minutes.

A timeout value of `0` indicates that the channel's default timeout value should be used. This default is configured using the <a href="dbchannel_set_timeout.md" class="xref" title="DB_CHANNEL-&gt;set_timeout()">DB_CHANNEL-&gt;set_timeout()</a> method.

#### flags

This parameter must be set to either `DB_MULTIPLE` or 0.

If there is a possibility that the response can consist of multiple DBT handles, specify `DB_MULTIPLE` to this parameter. In that case, the response buffer is formatted for bulk operations.

### Errors

The `DB_CHANNEL->send_request()` method may fail and return one of the following non-zero errors:

#### DB_BUFFER_SMALL

`DB_MULTIPLE` was not specified for the response DBT, but the remote site sent a response consisting of more than one DBT; or a buffer supplied using `DB_DBT_USERMEM` was not large enough to contain the message response.

#### DB_NOSERVER

A message was sent to a remote site that has not configured a message dispatch call-back function. Use the <a href="repmgr_msg_dispatch.md" class="xref" title="DB_ENV-&gt;repmgr_msg_dispatch()">DB_ENV-&gt;repmgr_msg_dispatch()</a> method at every site belonging to the replication group to configure a message dispatch call-back function.

#### EINVAL

If this method is called from a Base API application, or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
