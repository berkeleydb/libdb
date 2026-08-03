---
title: "Chapter 7. Standard Template Library API"
api-name: "Chapter 7. Standard Template Library API"
source: docs/programmer_reference/stl.html
---
## Chapter 7. Standard Template Library API

**Table of Contents**

<span class="sect1"> [Dbstl introduction](stl.md#stl_intro) </span>

<span class="sect2"> [Standards compatible](stl.md#stl_intro_stdcompat) </span>

<span class="sect2"> [Performance overhead](stl.md#stl_intro_performance) </span>

<span class="sect2"> [Portability](stl.md#stl_intro_portability) </span>

<span class="sect1"> [Dbstl typical use cases](stl_usecase.md) </span>

<span class="sect1"> [Dbstl examples](stl_examples.md) </span>

<span class="sect1"> [Berkeley DB configuration](stl_db_usage.md) </span>

<span class="sect2"> [Registering database and environment handles](stl_db_usage.md#idp51381760) </span>

<span class="sect2"> [Truncate requirements](stl_db_usage.md#idp51405208) </span>

<span class="sect2"> [Auto commit support](stl_db_usage.md#idp51416888) </span>

<span class="sect2"> [Database and environment identity checks](stl_db_usage.md#idp51379224) </span>

<span class="sect2"> [Products, constructors and configurations](stl_db_usage.md#idp51415360) </span>

<span class="sect1"> [Using advanced Berkeley DB features with dbstl](stl_db_advanced_usage.md) </span>

<span class="sect2"> [Using bulk retrieval iterators](stl_db_advanced_usage.md#idp51421384) </span>

<span class="sect2"> [Using the DB_RMW flag](stl_db_advanced_usage.md#idp51410312) </span>

<span class="sect2"> [Using secondary index database and secondary containers](stl_db_advanced_usage.md#idp51398048) </span>

<span class="sect1"> [Using transactions in dbstl](stl_txn_usage.md) </span>

<span class="sect1"> [Using dbstl in multithreaded applications](stl_mt_usage.md) </span>

<span class="sect1"> [Working with primitive types](stl_primitive_rw.md) </span>

<span class="sect2"> [Storing strings](stl_primitive_rw.md#idp51467888) </span>

<span class="sect1"> [Store and Retrieve data or objects of complex types](stl_complex_rw.md) </span>

<span class="sect2"> [Storing varying length objects](stl_complex_rw.md#idp51458752) </span>

<span class="sect2"> [Storing arbitrary sequences](stl_complex_rw.md#idp51477944) </span>

<span class="sect2"> [Notes](stl_complex_rw.md#idp51524696) </span>

<span class="sect1"> [Dbstl persistence](stl_persistence.md) </span>

<span class="sect2"> [Direct database get](stl_persistence.md#directdbget) </span>

<span class="sect2"> [Change persistence](stl_persistence.md#chg_persistence) </span>

<span class="sect2"> [Object life time and persistence](stl_persistence.md#obj_life_persistence) </span>

<span class="sect1"> [Dbstl container specific notes](stl_container_specific.md) </span>

<span class="sect2"> [db_vector specific notes](stl_container_specific.md#idp51492808) </span>

<span class="sect2"> [Associative container specific notes](stl_container_specific.md#idp51561456) </span>

<span class="sect1"> [Using dbstl efficiently](stl_efficienct_use.md) </span>

<span class="sect2"> [Using iterators efficiently](stl_efficienct_use.md#idp51530568) </span>

<span class="sect2"> [Using containers efficiently](stl_efficienct_use.md#idp51530352) </span>

<span class="sect1"> [Dbstl memory management](stl_memory_mgmt.md) </span>

<span class="sect2"> [Freeing memory](stl_memory_mgmt.md#idp51564672) </span>

<span class="sect2"> [Type specific notes](stl_memory_mgmt.md#idp51569240) </span>

<span class="sect1"> [Dbstl miscellaneous notes](stl_misc.md) </span>

<span class="sect2"> [Special notes about trivial methods](stl_misc.md#idp51587208) </span>

<span class="sect2"> [Using correct container and iterator public types](stl_misc.md#idp51603304) </span>

<span class="sect1"> [Dbstl known issues](stl_known_issues.md) </span>

## Dbstl introduction

<span class="sect2"> [Standards compatible](stl.md#stl_intro_stdcompat) </span>

<span class="sect2"> [Performance overhead](stl.md#stl_intro_performance) </span>

<span class="sect2"> [Portability](stl.md#stl_intro_portability) </span>

Dbstl is a C++ STL style API that provides for Berkeley DB usage. It allows for the storage and retrieval of data/objects of any type using Berkeley DB databases, but with an interface that mimics that of C++ STL containers. Dbstl provides access to all of the functionality of Berkeley DB available via this STL-style API.

With proper configuration, dbstl is able to store/retrieve any complex data types. There is no need to perform repetitive marshalling and unmarshalling of data. Dbstl also properly manages the life-cycle of all Berkeley DB structures and objects. All example methods referred to in this chapter can be found in the StlAdvancedFeaturesExample class in the \$DbSrc/examples_stl/StlAdvancedFeatures.cpp file, and you can build the example in \$DbSrc/build_unix directory like this: make exstl_advancedfeatures, where DbSrc is the source directory for Berkeley DB.

### Standards compatible

Dbstl is composed of many container and iterator class templates. These containers and iterators correspond exactly to each container and iterator available in the C++ STL API, including identical sets of methods. This allows existing algorithms, functions and container-adapters for C++ STL to use dbstl containers through its standard iterators. This means that existing STL code can manipulate Berkeley DB databases. As a result, existing C++ STL code can very easily use dbstl to gain persistence and transaction guarantees.

### Performance overhead

Because dbstl uses C++ template technologies, its performance overhead is minimal.

The dbstl API performs almost equally to the C API, as measured by two different implementations of the TPC-B benchmark: `ex_tpcb` and `exstl_tpcb`.

### Portability

The degree to which dbstl is portable to a new platform is determined by whether Berkeley DB is available on the platform, as well as whether an appropriate C++ compiler is available on the platform.

For information on porting Berkeley DB to new platforms, see the *Berkeley DB Porting Guide*.

Almost all the advanced C++ template features are used in dbstl, including:

- member function templates

- member function template overloads

- partial specialization

- default template parameters.

For this reason, you need a standards-compatible C++ compiler to build dbstl. As of this writing, the following compilers are known to build dbstl successfully:

- MSVC8

- gcc3.4.4 and above

- Intel C++ 9 and above

For \*nix platforms, if you can successfully configure your Berkeley DB build script with `--enable-stl`, then you should be able to successfully build dbstl library and application code using it.

Besides its own test suite, dbstl has also been tested against, and passes, the following test suites:

- MS STL test suite

- SGI STL test suite
