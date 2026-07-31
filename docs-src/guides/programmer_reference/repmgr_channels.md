---
title: "Using Replication Manager message channels"
api-name: "Using Replication Manager message channels"
source: docs/programmer_reference/repmgr_channels.html
---
## Using Replication Manager message channels

<span class="sect2"> [DB_CHANNEL](repmgr_channels.md#dbchannel_class) </span>

<span class="sect2"> [Sending messages over a message channel](repmgr_channels.md#dbchannel_send) </span>

<span class="sect2"> [Receiving messages](repmgr_channels.md#dbchannel_receive) </span>

The various sites comprising a replication group frequently need to communicate with one another. Mostly, these messages are handled for you internally by the Replication Manager. However, your application may have a requirement to pass messages beyond what the Replication Manager requires in order to satisfy its own internal workings.

For this reason, you can access and use the Replication Manager's internal message channels. You do this by using the `DB_CHANNEL` class, and by implementing a message handling function on each of your sites.

Note that an example of using Replication Manager message channels is available in the distribution. See <a href="rep_ex_chan.md" class="xref" title="Ex_rep_chan: a Replication Manager channel example">Ex_rep_chan: a Replication Manager channel example</a> for more information.

### DB_CHANNEL

The `DB_CHANNEL` class provides a series of methods which allow you to send messages to the other sites in your replication group. You create a `DB_CHANNEL` handle using the <a href="../../api/c/repmgr_channel.md" class="olink">DB_ENV-&gt;repmgr_channel()</a> method. When you are done with the handle, close it using the <a href="../../api/c/dbchannel_close.md" class="olink">DB_CHANNEL-&gt;close()</a> method. A closed handle must never be accessed again. Note that all channel handles should be closed before the associated environment handle is closed. Also, allow all message operations to complete on the channel before closing the handle.

When you create a `DB_CHANNEL` handle, you indicate what channel you want to use. Possibilities are:

- The numerical env ID of a remote site in the replication group.

- `DB_EID_MASTER`

  Messages sent on this channel are sent only to the master site. Note that messages are always sent to the current master, even if the master has changed since the channel was opened.

  If the local site is the master, then sending messages on this channel will result in the local site receiving those messages echoed back to itself.

### Sending messages over a message channel

You can send any message you want over a message channel. The message can be as simple as a character string and as complex as a large data structure. However, before you can send the message, you must encapsulate it within one or more <a href="../../api/c/dbt.md" class="olink">DBT</a>s. This means <a href="am_misc_struct.md" class="link" title="Storing C/C++ structures/objects">marshaling the message</a> if it is contained within a complex data structure.

The methods that you use to send messages all accept an array of <a href="../../api/c/dbt.md" class="olink">DBT</a>s. This means that in most circumstances it is perfectly acceptable to send multi-part messages.

Messages may be sent either asynchronously or synchronously. To send a message asynchronously, use the <a href="../../api/c/dbchannel_send_msg.md" class="olink">DB_CHANNEL-&gt;send_msg()</a> method. This method sends its message and then immediately returns without waiting for any sort of a response.

To send a message synchronously, use the <a href="../../api/c/dbchannel_send_request.md" class="olink">DB_CHANNEL-&gt;send_request()</a> method. This method blocks until it receives a response from the site to which it sent the message (or until a timeout threshold is reached).

#### Message Responses

Message responses are required if a message is sent on a channel using the <a href="../../api/c/dbchannel_send_request.md" class="olink">DB_CHANNEL-&gt;send_request()</a> method. That method accepts the address of a single <a href="../../api/c/dbt.md" class="olink">DBT</a> which is used to receive the response from the remote site.

Message responses are encapsulated in a single <a href="../../api/c/dbt.md" class="olink">DBT</a>. The response can be anything from a complex data structure, to a string, to a simple type, to no information at all. In the latter case, receipt of the <a href="../../api/c/dbt.md" class="olink">DBT</a> is sufficient to indicate that the request was received at the remote site.

Responses are sent back from the remote system using its message handling function. Usually that function calls <a href="../../api/c/dbchannel_send_msg.md" class="olink">DB_CHANNEL-&gt;send_msg()</a> to send a single response.

The response must be contained in a single <a href="../../api/c/dbt.md" class="olink">DBT</a>. If a multi-part response is required by the application, you can configure the response <a href="../../api/c/dbt.md" class="olink">DBT</a> that you provide to <a href="../../api/c/dbchannel_send_request.md" class="olink">DB_CHANNEL-&gt;send_request()</a> for <a href="am_misc_bulk.md" class="link" title="Retrieving and updating records in bulk">bulk operations</a>.

### Receiving messages

Messages received at a remote site are handled using a callback function. This function is configured for the local environment using the <a href="../../api/c/repmgr_msg_dispatch.md" class="olink">DB_ENV-&gt;repmgr_msg_dispatch()</a> method. For best results, the message dispatch function should be configured for the local environment before replication is started. In this way, you do not run the risk of missing messages sent after replication has started but before the message dispatch function is configured for the environment.

The callback configured by <a href="../../api/c/repmgr_msg_dispatch.md" class="olink">DB_ENV-&gt;repmgr_msg_dispatch()</a> accepts four parameters of note:

- A response channel. This is the channel the function will use to response to the message, if a response is required. To respond to the message, the function uses the <a href="../../api/c/dbchannel_send_msg.md" class="olink">DB_CHANNEL-&gt;send_msg()</a> method.

- An array of <a href="../../api/c/dbt.md" class="olink">DBT</a>s. These hold the message that this function must handle.

- A numerical value that indicates how many elements the previously described array holds.

- A flag that indicates whether the message requires a response. If the flag is set to `DB_REPMGR_NEED_RESPONSE`, then the function should send a single <a href="../../api/c/dbt.md" class="olink">DBT</a> in response using the channel provided to this function, and the <a href="../../api/c/dbchannel_send_msg.md" class="olink">DB_CHANNEL-&gt;send_msg()</a> method.

For an example of using this callback, see the `operation_dispatch()` function, which is available with the <a href="rep_ex_chan.md" class="link" title="Ex_rep_chan: a Replication Manager channel example">ex_rep_chan example</a> in your product distribution.
