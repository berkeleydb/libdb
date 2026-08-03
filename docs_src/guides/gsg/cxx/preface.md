---
title: "Preface"
api-name: "Preface"
source: docs/gsg/CXX/preface.html
---
## Preface

**Table of Contents**

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

Welcome to Berkeley DB (DB). This document introduces Berkeley DB 11<span class="emphasis">*g*</span> Release 2, which provides DB library version 11.2.5.3.

This document is intended to provide a rapid introduction to the DB API set and related concepts. The goal of this document is to provide you with an efficient mechanism with which you can evaluate DB against your project's technical requirements. As such, this document is intended for C++ developers and senior software architects who are looking for an in-process data management solution. No prior experience with Berkeley DB is expected or required.

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

In some situations, programming examples are updated from one chapter to the next. When this occurs, the new code is presented in **`monospaced bold`** font. For example:

``` c
typedef struct vendor {
    char name[MAXFIELD];             // Vendor name
    char street[MAXFIELD];           // Street name and number
    char city[MAXFIELD];             // City
    char state[3];                   // Two-digit US state code
    char zipcode[6];                 // US zipcode
    char phone_number[13];           // Vendor phone number
    char sales_rep[MAXFIELD];        // Name of sales representative
    char sales_rep_phone[MAXFIELD];  // Sales rep's phone number 
} VENDOR; 
```

### Note

Finally, notes of interest are represented using a note block such as this.
