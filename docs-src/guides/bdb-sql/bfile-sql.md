---
title: "BFILE SQL Objects and Functions"
api-name: "BFILE SQL Objects and Functions"
source: docs/bdb-sql/bfile-sql.html
---
## BFILE SQL Objects and Functions

<span class="sect2"> [BFILE_CREATE_DIRECTORY](bfile-sql.md#bfile_create_directory) </span>

<span class="sect2"> [BFILE_REPLACE_DIRECTORY](bfile-sql.md#bfile_replace_directory) </span>

<span class="sect2"> [BFILE_DROP_DIRECTORY](bfile-sql.md#bfile_drop_directory) </span>

<span class="sect2"> [BFILE_NAME](bfile-sql.md#bfile_name) </span>

<span class="sect2"> [BFILE_FULLPATH](bfile-sql.md#bfile_fullpath) </span>

<span class="sect2"> [BFILE_OPEN](bfile-sql.md#bfile_open) </span>

<span class="sect2"> [BFILE_READ](bfile-sql.md#bfile_read) </span>

<span class="sect2"> [BFILE_CLOSE](bfile-sql.md#bfile_close) </span>

<span class="sect2"> [BFILE_SIZE](bfile-sql.md#bfile_size) </span>

When the BFILE extension is enabled, you can create a `DIRECTORY` object. These objects are required before you can store a pointer to a file in a `BFILE` column.

`DIRECTORY` objects are stored in a special table called `BFILE_DIRECTORY`. This table is automatically created for you when it is needed. You should <span class="emphasis">*not*</span> manually create this table.

You manage `DIRECTORY` objects using the following SQL functions:

|  |
|----|
| <a href="bfile-sql.md#bfile_create_directory" class="xref" title="BFILE_CREATE_DIRECTORY">BFILE_CREATE_DIRECTORY</a> |
| <a href="bfile-sql.md#bfile_replace_directory" class="xref" title="BFILE_REPLACE_DIRECTORY">BFILE_REPLACE_DIRECTORY</a> |
| <a href="bfile-sql.md#bfile_drop_directory" class="xref" title="BFILE_DROP_DIRECTORY">BFILE_DROP_DIRECTORY</a> |

The following sections describe the SQL functions that you can use when the BFILE extension is enabled.

### BFILE_CREATE_DIRECTORY

``` c
BFILE_CREATE_DIRECTORY(directory, path)
```

Creates a `DIRECTORY` object as a path. The specified path must not already exist, or `Directory already exists` is returned.

### BFILE_REPLACE_DIRECTORY

``` c
BFILE_REPLACE_DIRECTORY(directory, path)
```

Replaces the named `DIRECTORY` object using the specified path. If the object does not exist, `Directory does not exist` is returned.

### BFILE_DROP_DIRECTORY

``` c
BFILE_DROP_DIRECTORY(directory)
```

Drops the named `DIRECTORY` object. If the object does not exist, `Directory does not exist` is returned.

### BFILE_NAME

``` c
BFILE_NAME(directory, filename)
```

Returns the BFILE locator.

### BFILE_FULLPATH

``` c
BFILE_FULLPATH(column)
```

Returns the full path.

### BFILE_OPEN

``` c
BFILE_OPEN(column)
```

Extracts the directory and file names from the BFILE locator, and then opens that file. On success, a BFILE handle is returned. Otherwise, `0` is returned.

### BFILE_READ

``` c
BFILE_READ(BFILE handle, amt, offset)
```

Reads at most `amt` data from the BFILE handle, starting at `offset`. On success, `Data` is returned. Otherwise, `0` is returned to indicate that no more valid data is available.

### BFILE_CLOSE

``` c
BFILE_CLOSE(BFILE handle)
```

Closes the BFILE handle.

### BFILE_SIZE

``` c
BFILE_SIZE(column)
```

Returns the size of the BFILE. On success, the size is returned. Otherwise, -1 is returned.
