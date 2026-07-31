---
title: "Preface"
api-name: "Preface"
source: docs/api_reference/STL/preface.html
---
## Preface

Welcome to Berkeley DB 11<span class="emphasis">*g*</span> Release 2 (DB). This document describes the C++ STL API for DB library version 11.2.5.3. It is intended to describe the DB API, including all classes, methods, and functions. As such, this document is intended for C++ developers who are actively writing or maintaining applications that make use of DB databases.

## Conventions Used in this Book

The following typographical conventions are used within in this manual:

Class names are represented in `monospaced font`, as are `method names`. For example: "`Db::open()` is a `Db` class method."

Variable or non-literal text is presented in <span class="emphasis">*italics*</span>. For example: "Go to your <span class="emphasis">*DB_INSTALL*</span> directory."

Program examples are displayed in a `monospaced font` on a shaded background. For example:

``` c
typedef struct vendor {
    char name[MAXFIELD];             // Vendor name
    char street[MAXFIELD];           // Street name and number
    char city[MAXFIELD];             // City
    char state[3];                   // Two-digit US state code
    char zipcode[6];                 // US zipcode
    char phone_number[13];           // Vendor phone number
} VENDOR; 
```

### Note

Finally, notes of interest are represented using a note block such as this.
