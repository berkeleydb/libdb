---
title: "BFILE C/C++ Objects and Functions"
api-name: "BFILE C/C++ Objects and Functions"
source: docs/bdb-sql/bfile-c.html
---
## BFILE C/C++ Objects and Functions

<span class="sect2"> [sqlite3_column_bfile](bfile-c.md#sqlite3_column_bfile) </span>

<span class="sect2"> [sqlite3_bfile_open](bfile-c.md#sqlite3_bfile_open) </span>

<span class="sect2"> [sqlite3_bfile_close](bfile-c.md#sqlite3_bfile_close) </span>

<span class="sect2"> [sqlite3_bfile_is_open](bfile-c.md#sqlite3_bfile_is_open) </span>

<span class="sect2"> [sqlite3_bfile_read](bfile-c.md#sqlite3_bfile_read) </span>

<span class="sect2"> [sqlite3_bfile_file_exists](bfile-c.md#sqlite3_bfile_file_exists) </span>

<span class="sect2"> [sqlite3_bfile_size](bfile-c.md#sqlite3_bfile_size) </span>

<span class="sect2"> [sqlite3_bfile_final](bfile-c.md#sqlite3_bfile_final) </span>

The BFILE extension can optionally make available to you some additional C language data types and functions for use with the SQLite C/C++ interface. These are available to you only if you take the proper steps when you compile Berkeley DB. See the *Berkeley DB Installation and Build Guide* for more information.

Once enabled, the BFILE C extension makes the following new structure available to you:

``` c
typedef struct sqlite3_bfile sqlite3_bfile;
```

This structure serves as the BFILE handle when you are using the BFILE extension along with the SQLite C/C++ interface.

In addition to the new structure, you can also use the following new C functions:

### sqlite3_column_bfile

``` c
int 
sqlite3_column_bfile(sqlite3_stmt *pStmt, int iCol, 
                     sqlite3_bfile **ppBfile); 
```

Returns a result set from a query against a column of type BFILE.

On success, `SQLITE_OK` is returned and the new BFILE handle is written to **ppBfile**. Otherwise, `SQLITE_ERROR` is returned.

Parameters are:

- **pStmt**

  Pointer to the prepared statement that the function is evaluating. The statement is created using `sqlite3_prepare_v2()` or one of its variants.

  If this statement does not point to a valid row, the result is undefined.

- **iCol**

  Index of the column for which information should be returned. The left-most column of the result set is index `0`. Use `sqlite3_column_count()` to discover the number of columns in the result set.

  If the column index is out of range, the result is undefined.

- **ppBfile**

  The BFILE handle that you are using for the query. This pointer is valid only until `sqlite3_step()`, `sqlite3_reset()` or `sqlite3_finalize()` have been called.

  The memory space used to hold this handle is freed by <a href="bfile-c.md#sqlite3_bfile_final" class="xref" title="sqlite3_bfile_final">sqlite3_bfile_final</a> Do not pass these pointers to `sqlite3_free()`.

This function can be called successfully only if all of the following conditions are true. If any of the following are not true, the result is undefined:

- The most recent call to `sqlite3_step()` has returned `SQLITE_ROW`.

- Neither `sqlite3_reset()` nor `sqlite3_finalize()` have been called since the last time `sqlite3_step()` was called.

- `sqlite3_step()`, `sqlite3_reset()` or `sqlite3_finalize()` have not been called from a different thread while this routine is pending.

### sqlite3_bfile_open

``` c
int
sqlite3_bfile_open(sqlite3_bfile *pBfile); 
```

Opens a file for incremental read.

On success, `SQLITE_OK` is returned. Otherwise, `SQLITE_ERROR` is returned.

To avoid a resource leak, every opened BFILE handle should eventually be closed with the <a href="bfile-c.md#sqlite3_bfile_close" class="xref" title="sqlite3_bfile_close">sqlite3_bfile_close</a> function. Note that **pBfile** is always initialized such that it is always safe to invoke `sqlite_bfile_close()` against it, regardless of the success or failure of this function.

### sqlite3_bfile_close

``` c
int
sqlite3_bfile_close(sqlite3_bfile *pBfile); 
```

Closes an open BFILE handle. The BFILE is closed unconditionally. Even if this function returns an error, the BFILE is still closed.

Calling this routine with a null pointer (such as would be returned by failed call to sqlite3_column_bfile()) is a harmless non-operation.

On success, `SQLITE_OK` is returned. Otherwise, `SQLITE_ERROR` is returned.

### sqlite3_bfile_is_open

``` c
int
sqlite3_bfile_is_open(sqlite3_bfile *pBfile, int *open); 
```

Checks whether a BFILE handle is open. The `open` parameter is set to 1 if the file is open, otherwise it is 0.

On success, `SQLITE_OK` is returned. Otherwise, `SQLITE_ERROR` is returned.

### sqlite3_bfile_read

``` c
int 
sqlite3_bfile_read(sqlite3_bfile *pBfile, void *oBuff, int nSize, 
                   int iOffset, int *nRead); 
```

This function is used to read data from an opened BFILE handle into a caller-supplied buffer.

On success, `SQLITE_OK` is returned, the data that has been read is written to the output buffer, **oBuff**, and the amount of data written to the buffer is recorded in **nRead**. Otherwise, `SQLITE_ERROR` is returned.

Parameters are:

- **pBfile**

  The BFILE handle from which the data is read.

  This function only works on a BFILE handle which has been created by a prior successful call to <a href="bfile-c.md#sqlite3_bfile_open" class="xref" title="sqlite3_bfile_open">sqlite3_bfile_open</a> and which has not been closed by <a href="bfile-c.md#sqlite3_bfile_close" class="xref" title="sqlite3_bfile_close">sqlite3_bfile_close</a>. Passing any other pointer in to this function results in undefined and probably undesirable behavior.

- **oBuff**

  The buffer used to contain the data that is read from **pBfile**. It must be at least **nSize** bytes in size.

- **nSize**

  The amount of data, in bytes, to read from the BFILE.

- **iOffset**

  The offset from the beginning of the file where the read operation is to begin.

- **nRead**

  Contains the amount of data, in bytes, actually written to buffer **oBuff** once the read operation is completed.

### Note

The size of the BFILE can be determined using the <a href="bfile-c.md#sqlite3_bfile_size" class="xref" title="sqlite3_bfile_size">sqlite3_bfile_size</a> function.

### sqlite3_bfile_file_exists

``` c
int
sqlite3_bfile_file_exists(sqlite3_bfile *pBfile, int *exist); 
```

Checks whether a BFILE exists. The `exists` parameter is set to 1 if the file is exists, otherwise it is 0.

On success, `SQLITE_OK` is returned. Otherwise, `SQLITE_ERROR` is returned.

### sqlite3_bfile_size

``` c
int
sqlite3_bfile_size(sqlite3_bfile *pBfile, off_t *size); 
```

Returns the size of the BFILE, in bytes.

On success, `SQLITE_OK` is returned, and **size** is set to the size of the BFILE, in bytes. Otherwise, `SQLITE_ERROR` is returned.

This function only works on a BFILE handle which has been created by a prior successful call to <a href="bfile-c.md#sqlite3_column_bfile" class="xref" title="sqlite3_column_bfile">sqlite3_column_bfile</a> and which has not been finalized by <a href="bfile-c.md#sqlite3_bfile_final" class="xref" title="sqlite3_bfile_final">sqlite3_bfile_final</a>. Passing any other pointer in to this function results in undefined and probably undesirable behavior.

### sqlite3_bfile_final

``` c
int
sqlite3_bfile_final(sqlite3_bfile *pBfile); 
```

Frees a BFILE handle.

On success, `SQLITE_OK` is returned. Otherwise, `SQLITE_ERROR` is returned.
