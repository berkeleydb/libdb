---
title: "Chapter 2.  System Installation Notes"
api-name: "Chapter 2.  System Installation Notes"
source: docs/installation/install.html
---
## Chapter 2.  System Installation Notes

**Table of Contents**

<span class="sect1"> [File utility /etc/magic information](install.md#install_file) </span>

<span class="sect2"> [Magic information](install.md#magic) </span>

<span class="sect2"> [Big-endian magic information](install.md#big-endian) </span>

<span class="sect2"> [Little-endian magic information](install.md#little-endian) </span>

<span class="sect1"> [Building with multiple versions of Berkeley DB](install_multiple.md) </span>

## File utility /etc/magic information

<span class="sect2"> [Magic information](install.md#magic) </span>

<span class="sect2"> [Big-endian magic information](install.md#big-endian) </span>

<span class="sect2"> [Little-endian magic information](install.md#little-endian) </span>

The `file`(1) utility is a UNIX utility that examines and classifies files, based on information found in its database of file types, the /etc/magic file. The following information may be added to your system's /etc/magic file to enable `file`(1) to correctly identify Berkeley DB database files.

The `file`(1) utility `magic`(5) information for the standard System V UNIX implementation of the `file`(1) utility is included in the Berkeley DB distribution for both big-endian (for example, Sparc) and little-endian (for example, x86) architectures. See <a href="install.md#big-endian" class="xref" title="Big-endian magic information">Big-endian magic information</a> and <a href="install.md#little-endian" class="xref" title="Little-endian magic information">Little-endian magic information</a> respectively for this information.

The `file`(1) utility `magic`(5) information for Release 3.X of Ian Darwin's implementation of the file utility (as distributed by FreeBSD and most Linux distributions) is included in the Berkeley DB distribution. This `magic.txt` information is correct for both big-endian and little-endian architectures. See the next section for this information.

### Magic information

``` c
# Berkeley DB

# Ian Darwin's file /etc/magic files: big/little-endian version.

# Hash 1.85/1.86 databases store metadata in network byte order.
# Btree 1.85/1.86 databases store the metadata in host byte order.
# Hash and Btree 2.X and later databases store the metadata in 
# host byte order.

0   long    0x00061561  Berkeley DB
>8   belong  4321
>>4   belong  >2       1.86
>>4   belong  <3       1.85
>>4   belong  >0       (Hash, version %d, native byte-order)
>8   belong  1234
>>4   belong  >2       1.86
>>4   belong  <3       1.85
>>4   belong  >0       (Hash, version %d, little-endian)

0   belong  0x00061561  Berkeley DB
>8   belong  4321
>>4   belong  >2       1.86
>>4   belong  <3       1.85
>>4   belong  >0       (Hash, version %d, big-endian)
>8   belong  1234
>>4   belong  >2       1.86
>>4   belong  <3       1.85
>>4   belong  >0       (Hash, version %d, native byte-order)

0   long    0x00053162  Berkeley DB 1.85/1.86
>4   long    >0       (Btree, version %d, native byte-order)
0   belong  0x00053162  Berkeley DB 1.85/1.86
>4   belong  >0       (Btree, version %d, big-endian)
0   lelong  0x00053162  Berkeley DB 1.85/1.86
>4   lelong  >0       (Btree, version %d, little-endian)

12  long    0x00061561  Berkeley DB
>16  long    >0       (Hash, version %d, native byte-order)
12  belong  0x00061561  Berkeley DB
>16  belong  >0       (Hash, version %d, big-endian)
12  lelong  0x00061561  Berkeley DB
>16  lelong  >0       (Hash, version %d, little-endian)

12  long    0x00053162  Berkeley DB
>16  long    >0       (Btree, version %d, native byte-order)
12  belong  0x00053162  Berkeley DB
>16  belong  >0       (Btree, version %d, big-endian)
12  lelong  0x00053162  Berkeley DB
>16  lelong  >0       (Btree, version %d, little-endian)

12  long    0x00042253  Berkeley DB
>16  long    >0       (Queue, version %d, native byte-order)
12  belong  0x00042253  Berkeley DB
>16  belong  >0       (Queue, version %d, big-endian)
12  lelong  0x00042253  Berkeley DB
>16  lelong  >0       (Queue, version %d, little-endian)

12  long    0x00040988  Berkeley DB
>16  long    >0       (Log, version %d, native byte-order)
12  belong  0x00040988  Berkeley DB
>16  belong  >0       (Log, version %d, big-endian)
12  lelong  0x00040988  Berkeley DB
>16  lelong  >0       (Log, version %d, little-endian) 
```

### Big-endian magic information

``` c
# Berkeley DB

# System V /etc/magic files: big-endian version.

# Hash 1.85/1.86 databases store metadata in network byte order.
# Btree 1.85/1.86 databases store the metadata in host byte order.
# Hash and Btree 2.X and later databases store the metadata in 
# host byte order.

0   long    0x00053162  Berkeley DB 1.85/1.86 (Btree,
>4   long    0x00000002  version 2,
>4   long    0x00000003  version 3,
>0   long    0x00053162  native byte-order)

0   long    0x62310500  Berkeley DB 1.85/1.86 (Btree,
>4   long    0x02000000  version 2,
>4   long    0x03000000  version 3,
>0   long    0x62310500  little-endian)

12  long    0x00053162  Berkeley DB (Btree,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>12  long    0x00053162  native byte-order)

12  long    0x62310500  Berkeley DB (Btree,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>12  long    0x62310500  little-endian)

0   long    0x00061561  Berkeley DB
>4   long    >2       1.86
>4   long    <3       1.85
>0   long    0x00061561  (Hash,
>4   long    2       version 2,
>4   long    3       version 3,
>8   long    0x000004D2  little-endian)
>8   long    0x000010E1  native byte-order)

12  long    0x00061561  Berkeley DB (Hash,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>12  long    0x00061561  native byte-order)

12  long    0x61150600  Berkeley DB (Hash,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>12  long    0x61150600  little-endian)

12  long    0x00042253  Berkeley DB (Queue,
>16  long    0x00000001  version 1,
>16  long    0x00000002  version 2,
>16  long    0x00000003  version 3,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>12  long    0x00042253  native byte-order)

12  long    0x53220400  Berkeley DB (Queue,
>16  long    0x01000000  version 1,
>16  long    0x02000000  version 2,
>16  long    0x03000000  version 3,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>12  long    0x53220400  little-endian)

12  long    0x00040988  Berkeley DB (Log,
>16  long    0x00000001  version 1,
>16  long    0x00000002  version 2,
>16  long    0x00000003  version 3,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>16  long    0x0000000a  version 10,
>16  long    0x0000000b  version 11,
>16  long    0x0000000c  version 12,
>16  long    0x0000000d  version 13,
>16  long    0x0000000e  version 14,
>16  long    0x0000000f  version 15,
>12  long    0x00040988  native byte-order)

12  long    0x88090400  Berkeley DB (Log,
>16  long    0x01000000  version 1,
>16  long    0x02000000  version 2,
>16  long    0x03000000  version 3,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>16  long    0x0a000000  version 10,
>16  long    0x0b000000  version 11,
>16  long    0x0c000000  version 12,
>16  long    0x0d000000  version 13,
>16  long    0x0e000000  version 14,
>16  long    0x0f000000  version 15,
>12  long    0x88090400  little-endian)
```

### Little-endian magic information

``` c
# Berkeley DB

# System V /etc/magic files: little-endian version.

# Hash 1.85/1.86 databases store metadata in network byte order.
# Btree 1.85/1.86 databases store the metadata in host byte order.
# Hash and Btree 2.X and later databases store the metadata in 
# host byte order.

0   long    0x00053162  Berkeley DB 1.85/1.86 (Btree,
>4   long    0x00000002  version 2,
>4   long    0x00000003  version 3,
>0   long    0x00053162  native byte-order)

0   long    0x62310500  Berkeley DB 1.85/1.86 (Btree,
>4   long    0x02000000  version 2,
>4   long    0x03000000  version 3,
>0   long    0x62310500  big-endian)

12  long    0x00053162  Berkeley DB (Btree,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>12  long    0x00053162  native byte-order)

12  long    0x62310500  Berkeley DB (Btree,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>12  long    0x62310500  big-endian)

0   long    0x61150600  Berkeley DB
>4   long    >0x02000000  1.86
>4   long    <0x03000000  1.85
>0   long    0x00061561  (Hash,
>4   long    0x02000000  version 2,
>4   long    0x03000000  version 3,
>8   long    0xD2040000  native byte-order)
>8   long    0xE1100000  big-endian)

12  long    0x00061561  Berkeley DB (Hash,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>12  long    0x00061561  native byte-order)

12  long    0x61150600  Berkeley DB (Hash,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>12  long    0x61150600  big-endian)

12  long    0x00042253  Berkeley DB (Queue,
>16  long    0x00000001  version 1,
>16  long    0x00000002  version 2,
>16  long    0x00000003  version 3,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>12  long    0x00042253  native byte-order)

12  long    0x53220400  Berkeley DB (Queue,
>16  long    0x01000000  version 1,
>16  long    0x02000000  version 2,
>16  long    0x03000000  version 3,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>12  long    0x53220400  big-endian)

12  long    0x00040988  Berkeley DB (Log,
>16  long    0x00000001  version 1,
>16  long    0x00000002  version 2,
>16  long    0x00000003  version 3,
>16  long    0x00000004  version 4,
>16  long    0x00000005  version 5,
>16  long    0x00000006  version 6,
>16  long    0x00000007  version 7,
>16  long    0x00000008  version 8,
>16  long    0x00000009  version 9,
>16  long    0x0000000a  version 10,
>16  long    0x0000000b  version 11,
>16  long    0x0000000c  version 12,
>16  long    0x0000000d  version 13,
>16  long    0x0000000e  version 14,
>16  long    0x0000000f  version 15,
>12  long    0x00040988  native byte-order)

12  long    0x88090400  Berkeley DB (Log,
>16  long    0x01000000  version 1,
>16  long    0x02000000  version 2,
>16  long    0x03000000  version 3,
>16  long    0x04000000  version 4,
>16  long    0x05000000  version 5,
>16  long    0x06000000  version 6,
>16  long    0x07000000  version 7,
>16  long    0x08000000  version 8,
>16  long    0x09000000  version 9,
>16  long    0x0a000000  version 10,
>16  long    0x0b000000  version 11,
>16  long    0x0c000000  version 12,
>16  long    0x0d000000  version 13,
>16  long    0x0e000000  version 14,
>16  long    0x0f000000  version 15,
>12  long    0x88090400  big-endian) 
```
