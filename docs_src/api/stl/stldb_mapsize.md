---
title: "size"
api-name: "size"
source: docs/api_reference/STL/stldb_mapsize.html
---
## size

### Function Details

``` c
size_type size(bool accurate=true) const
 
```

This function supports auto-commit.

#### Parameters

##### accurate

This function uses database's statistics to get the number of key/data pairs. The statistics mechanism will either scan the whole database to find the accurate number or use the number of last accurate scanning, and thus much faster. If there are millions of key/data pairs, the scanning can take some while, so in that case you may want to set the "accurate" parameter to false.

#### Return Value

Return the number of key/data pairs in the container.

### Group: Metadata Functions

These functions return metadata about the container.

### Class

<a href="db_map.md" class="link" title="Chapter 5.  Db_map">db_map</a>
