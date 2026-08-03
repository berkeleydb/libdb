---
title: "Repmgr Message Channels"
api-name: "Repmgr Message Channels"
source: docs/installation/upgrade_11gr2_52_repmgr_channels.html
---
## Repmgr Message Channels

<span class="sect2"> [New Functions](upgrade_11gr2_52_repmgr_channels.md#idp919280) </span>

Application components running at various sites within a replication group can now use the Replication Manager's existing TCP/IP communications infrastructure to send and process messages among themselves, using the `DB_CHANNEL` handle. <a href="../../api/c/repmgr_channel.md" class="olink">DB_ENV-&gt;repmgr_channel()</a> is used to create the `DB_CHANNEL` handle. <a href="../../api/c/dbchannel_send_msg.md" class="olink">DB_CHANNEL-&gt;send_msg()</a> and <a href="../../api/c/dbchannel_send_request.md" class="olink">DB_CHANNEL-&gt;send_request()</a> are used to send sychronous and asychronous messages that are handled by the function set by <a href="../../api/c/repmgr_msg_dispatch.md" class="olink">DB_ENV-&gt;repmgr_msg_dispatch()</a>. <a href="../../api/c/dbchannel_set_timeout.md" class="olink">DB_CHANNEL-&gt;set_timeout()</a> is used to configure channel time out, and <a href="../../api/c/dbchannel_close.md" class="olink">DB_CHANNEL-&gt;close()</a> closes the channel and frees resources held by it.

### New Functions

- <a href="../../api/c/repmgr_msg_dispatch.md" class="olink">DB_ENV-&gt;repmgr_msg_dispatch()</a>
- <a href="../../api/c/repmgr_channel.md" class="olink">DB_ENV-&gt;repmgr_channel()</a>
- <a href="../../api/c/dbchannel_send_msg.md" class="olink">DB_CHANNEL-&gt;send_msg()</a>
- <a href="../../api/c/dbchannel_send_request.md" class="olink">DB_CHANNEL-&gt;send_request()</a>
- <a href="../../api/c/dbchannel_set_timeout.md" class="olink">DB_CHANNEL-&gt;set_timeout()</a>
- <a href="../../api/c/dbchannel_close.md" class="olink">DB_CHANNEL-&gt;close()</a>
