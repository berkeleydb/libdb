---
title: "DB_ENV->repmgr_msg_dispatch()"
api-name: "DB_ENV->repmgr_msg_dispatch()"
source: docs/api_reference/C/repmgr_msg_dispatch.html
---
## DB_ENV-\>repmgr_msg_dispatch()

``` c
#include <db.h>

int
DB_ENV->repmgr_msg_dispatch(DB_ENV *env, 
        void (*msg_dispatch_fcn) (DB_ENV *env, DB_CHANNEL *channel, 
                                  DBT *request, u_int32_t nrequest,
                                  u_int32_t cb_flags), 
        u_int32_t flags);  
```

Sets the message dispatch function. This function is responsible for receiving messages sent from remote sites using either the <a href="dbchannel_send_msg.md" class="xref" title="DB_CHANNEL-&gt;send_msg()">DB_CHANNEL-&gt;send_msg()</a> or <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> methods. If the message received by this function was sent using the <a href="dbchannel_send_msg.md" class="xref" title="DB_CHANNEL-&gt;send_msg()">DB_CHANNEL-&gt;send_msg()</a> method then no response is required. If the message was sent using the <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> method, then this function must send a response using the <a href="dbchannel_send_msg.md" class="xref" title="DB_CHANNEL-&gt;send_msg()">DB_CHANNEL-&gt;send_msg()</a> method.

For best results, the `DB_ENV->repmgr_msg_dispatch()` method should be called before the Replication Manager has been started.

The `DB_ENV->repmgr_msg_dispatch()` method returns a non-zero error value on failure and 0 on success.

### Parameters

#### msg_dispatch_fcn

This parameter is the application-specific function used to handle messages sent over Replication Manager message channels. It takes four parameters:

- `channel`

  Provides the DB_CHANNEL to be used to send a response back to the originator of the message. If the message was sent by the remote site using <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> then this function should send a response back to the originator using the channel provided on this parameter. The message should be sent by calling <a href="dbchannel_send_msg.md" class="xref" title="DB_CHANNEL-&gt;send_msg()">DB_CHANNEL-&gt;send_msg()</a> exactly once.

  This channel is valid only during the current invocation of the dispatch function; it is destroyed when the dispatch function returns. The application may not save a copy of the pointer and use it later elsewhere. Methods that do not make sense in the context of a message dispatch function (such as <a href="dbchannel_send_request.md" class="xref" title="DB_CHANNEL-&gt;send_request()">DB_CHANNEL-&gt;send_request()</a> and <a href="dbchannel_close.md" class="xref" title="DB_CHANNEL-&gt;close()">DB_CHANNEL-&gt;close()</a>) will be rejected with `EINVAL`.

- `request`

  Array of DBTs containing the message received from the remote site.

- `nrequest`

  Specifies the number of elements in the `request` array.

- `cb_flags`

  This flag is `DB_REPMGR_NEED_RESPONSE` if the message requires a response. Otherwise, it is `0`.

This function does not return a value. If the function encounters an error, you can reflect the error back to the originator of the message by formatting an error message of your own design into the response.

#### flags

This parameter is currently unused, and must be set to 0.

### Errors

The `DB_ENV->repmgr_msg_dispatch()` method may fail and return one of the following non-zero errors:

#### EINVAL

If this method is called from a Base API application, or if an invalid flag value or parameter was specified.

### Class

<a href="env.md" class="link" title="Chapter 5.  The DB_ENV Handle">DB_ENV</a>

### See Also

<a href="rep.md#replist" class="xref" title="Replication and Related Methods">Replication and Related Methods</a>
