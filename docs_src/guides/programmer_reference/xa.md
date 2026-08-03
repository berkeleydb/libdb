---
title: "Chapter 13.  Distributed Transactions"
api-name: "Chapter 13.  Distributed Transactions"
source: docs/programmer_reference/xa.html
---
## Chapter 13.  Distributed Transactions

**Table of Contents**

<span class="sect1"> [Introduction](xa.md#xa_intro) </span>

<span class="sect1"> [Berkeley DB XA Implementation](ch13s02.md) </span>

<span class="sect1"> [Building a Global Transaction Manager](xa_build.md) </span>

<span class="sect2"> [Communicating with multiple Berkeley DB environments](xa_build.md#idp52778488) </span>

<span class="sect2"> [Recovering from GTM failure](xa_build.md#idp52779432) </span>

<span class="sect2"> [Managing the Global Transaction ID (GID) name space](xa_build.md#idp52703176) </span>

<span class="sect2"> [Maintaining state for each distributed transaction.](xa_build.md#idp52758336) </span>

<span class="sect2"> [Recovering from the failure of a single environment](xa_build.md#idp52777008) </span>

<span class="sect2"> [Recovering from GTM failure](xa_build.md#idp52779896) </span>

<span class="sect1"> [XA Introduction](xa_xa_intro.md) </span>

<span class="sect1"> [Configuring Berkeley DB with the Tuxedo System](xa_xa_config.md) </span>

<span class="sect2"> [Update the Resource Manager File in Tuxedo](xa_xa_config.md#idp52786896) </span>

<span class="sect2"> [Build the Transaction Manager Server](xa_xa_config.md#idp52812512) </span>

<span class="sect2"> [Update the UBBCONFIG File](xa_xa_config.md#idp52759288) </span>

<span class="sect1"> [Restrictions on XA Transactions](xa_xa_restrict.md) </span>

<span class="sect1"> [XA: Frequently Asked Questions](xa_faq.md) </span>

## Introduction

An application must use distributed transactions whenever it wants transactional semantics either across operations in multiple Berkeley DB environments (even if they are on the same machine) or across operations in Berkeley DB and some other database systems (for example, Oracle server). Berkeley DB provides support for distributed transactions using a two-phase commit protocol. In order to use the two-phase commit feature of Berkeley DB, an application must either implement its own global transaction manager or use an XA-compliant transaction manager such as Oracle Tuxedo (as Berkeley DB can act as an XA-compliant resource manager).

This chapter explains Berkeley DB's XA-compliant resource manager, which can be used in any X/Open distributed transaction processing system, and explains how to configure Oracle Tuxedo to use the Berkeley DB resource manager.
