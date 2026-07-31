---
title: "Dbstl typical use cases"
api-name: "Dbstl typical use cases"
source: docs/programmer_reference/stl_usecase.html
---
## Dbstl typical use cases

Among others, the following are some typical use cases where dbstl would be prefered over C++ STL:

- Working with a large amount of data, more than can reside in memory. Using C++ STL would force a number of page swaps, which will degrade performance. When using dbstl, data is stored in a database and Berkeley DB ensures the needed data is in memory, so that the overall performance of the machine is not slowed down.

- Familiar Interface. dbstl provides a familiar interface to Berkeley DB, hiding the marshalling and unmashalling details and automatically managing Berkeley DB structures and objects.

- Transaction semantics. dbstl provides the ACID properties (or a subset of the ACID properties) in addition to supporting all of the STL functionality.

- Concurrent access. Few (if any) existing C++ STL implementations support reading/writing to the same container concurrently, dbstl does.

- Object persistence. dbstl allows your application to store objects in a database, and use the objects across different runs of your application. dbstl is capable of storing complicated objects which are not located in a contiguous chunk of memory, with some user configurations.
