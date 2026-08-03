---
title: "Preface"
api-name: "Preface"
source: docs/gsg_db_rep/CXX/preface.html
---
## Preface

**Table of Contents**

<span class="sect1"> [Conventions Used in this Book](preface.md#conventions) </span>

<span class="sect1"> [For More Information](moreinfo.md) </span>

<span class="sect2"> [Contact Us](moreinfo.md#contact_us) </span>

This document describes how to write replicated applications for Berkeley DB 11<span class="emphasis">*g*</span> Release 2 (library version 11.2.5.3). The APIs used to implement replication in your application are described here. This book describes the concepts surrounding replication, the scenarios under which you might choose to use it, and the architectural requirements that a replication application has over a transactional application.

This book is aimed at the software engineer responsible for writing a replicated DB application.

This book assumes that you have already read and understood the concepts contained in the *Berkeley DB Getting Started with Transaction Processing* guide.

## Conventions Used in this Book

The following typographical conventions are used within in this manual:

Class names are represented in `monospaced font`, as are `method names`. For example: "`DbEnv::open()` is a `DbEnv` class method."

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

Finally, notes of special interest are represented using a note block such as this.
