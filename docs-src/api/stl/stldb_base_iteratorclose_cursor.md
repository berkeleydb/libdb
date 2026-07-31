---
title: "close_cursor"
api-name: "close_cursor"
source: docs/api_reference/STL/stldb_base_iteratorclose_cursor.html
---
## close_cursor

### Function Details

``` c
void close_cursor() const
 
```

Close its cursor.

If you are sure the iterator is no longer used, call this function so that its underlying cursor is closed before this iterator is destructed, potentially increase performance and concurrency. Note that the cursor is definitely closed at iterator destruction if you don't close it explicitly.

### Class

<a href="db_base_iterator.md" class="link" title="Chapter 10.  Db_base_iterator">db_base_iterator</a>
