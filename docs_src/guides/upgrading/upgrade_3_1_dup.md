---
title: "identical duplicate data items"
api-name: "identical duplicate data items"
source: docs/upgrading/upgrade_3_1_dup.html
---
## identical duplicate data items

In previous releases of Berkeley DB, it was not an error to store identical duplicate data items, or, for those that just like the way it sounds, duplicate duplicates. However, there were implementation bugs where storing duplicate duplicates could cause database corruption.

In this release, applications may store identical duplicate data items as long as the data items are unsorted. It is an error to attempt to store identical duplicate data items when duplicates are being stored in a sorted order. This restriction is expected to be lifted in a future release. See <a href="../../guides/programmer_reference/general_am_conf.md#am_conf_dup" class="olink">Duplicate data items</a> for more information.
