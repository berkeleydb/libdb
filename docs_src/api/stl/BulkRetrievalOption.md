---
title: "Chapter 27.  BulkRetrievalOption"
api-name: "Chapter 27.  BulkRetrievalOption"
source: docs/api_reference/STL/BulkRetrievalOption.html
---
## Chapter 27.  BulkRetrievalOption

Bulk retrieval configuration helper class.

Used by the begin() function of a container.

#### Public Members

| Member | Description |
|----|----|
| <a href="BulkRetrievalOption.md#stlBulkRetrievalOptionBulkRetrievalOption" class="xref" title="BulkRetrievalOption">BulkRetrievalOption</a> |  |
| <a href="stlBulkRetrievalOptionoperator_eq.md" class="xref" title="operator==">operator==</a> | Equality comparison. |
| <a href="stlBulkRetrievalOptionoperator_assign.md" class="xref" title="operator=">operator=</a> | Assignment operator. |
| <a href="stlBulkRetrievalOptionbulk_buf_size.md" class="xref" title="bulk_buf_size">bulk_buf_size</a> | Return the buffer size set to this object. |
| <a href="stlBulkRetrievalOptionbulk_retrieval.md" class="xref" title="bulk_retrieval">bulk_retrieval</a> | This function indicates that you need a bulk retrieval iterator, and it can be also used to optionally set the bulk read buffer size. |
| <a href="stlBulkRetrievalOptionno_bulk_retrieval.md" class="xref" title="no_bulk_retrieval">no_bulk_retrieval</a> | This function indicates that you do not need a bulk retrieval iterator. |

#### Group

<a href="dbstl_helper_classes.md" class="xref" title="Chapter 21.  Dbstl Helper Classes">Dbstl Helper Classes</a>

## BulkRetrievalOption

### Function Details

``` c
BulkRetrievalOption(Option bulk_retrieve1,
    u_int32_t bulk_buf_sz=DBSTL_BULK_BUF_SIZE)
 
```

### Class

<a href="BulkRetrievalOption.md" class="link" title="Chapter 27.  BulkRetrievalOption">BulkRetrievalOption</a>
