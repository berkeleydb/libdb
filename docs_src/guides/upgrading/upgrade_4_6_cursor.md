---
title: "C API cursor handle method names"
api-name: "C API cursor handle method names"
source: docs/upgrading/upgrade_4_6_cursor.html
---
## C API cursor handle method names

In the Berkeley DB 4.6 release, the C API <a href="../../api/c/dbc.md" class="olink">DBC</a> handle methods have been renamed for consistency with the C++ and Java APIs. The change is the removal of the leading "c\_" from the names, as follows:

<span class="term">DBC-\>c_close</span>  
Renamed DBC-\>close

<span class="term">DBC-\>c_count</span>  
Renamed DBC-\>count

<span class="term">DBC-\>c_del</span>  
Renamed DBC-\>del

<span class="term">DBC-\>c_dup</span>  
Renamed DBC-\>dup

<span class="term">DBC-\>c_get</span>  
Renamed DBC-\>get

<span class="term">DBC-\>c_pget</span>  
Renamed DBC-\>pget

<span class="term">DBC-\>c_put</span>  
Renamed DBC-\>put

The old <a href="../../api/c/dbc.md" class="olink">DBC</a> method names are deprecated but will continue for work for some number of future releases.
