---
title: "set_open_flags"
api-name: "set_open_flags"
source: docs/api_reference/C/set_open_flags_parameter.html
---
## set_open_flags

Initializes specific subsystems of the Berkeley DB environment.

The syntax of the entry in the <a href="../../guides/programmer_reference/env_db_config.md#env_db_config.DB_CONFIG" class="olink">DB_CONFIG</a> is a single line with the string `set_open_flags`, one or more whitespace characters, the method flag parameter as a string, optionally one or more whitespace characters, and the string `on` or `off`. If the optional string is omitted, the default is `on`; for example, `set_open_flags DB_INIT_REP` or `set_open_flags DB_INIT_REP on`. Because the DB_CONFIG file is read when the database environment is opened, it will silently overrule configuration done before that time.

The method flag parameters are as follows:

- DB_INIT_REP

  Enables/disables DB_INIT_REP in the DB_ENV-\>open method. For example:

  ``` c
  set_open_flags DB_INIT_REP on
  ```

  This enables initializing the replication subsystem. This subsystem should be used whenever an application plans on using replication. This setting overwrites the DB_INIT_REP flag passed from the application's DB_ENV-\>open method.

- DB_PRIVATE

  Enables/disables DB_PRIVATE in the DB_ENV-\>open method. For example:

  ``` c
  set_open_flags DB_PRIVATE on
  ```

  This enables region memory allocation from the heap instead of from memory backed by the filesystem or system shared memory. This flag implies the environment will only be accessed by a single process (although that process may be multithreaded). This flag has two effects on the Berkeley DB environment. First, all underlying data structures are allocated from per-process memory instead of from shared memory that is accessible to more than a single process. Second, mutexes are only configured to work between threads. This setting overwrites the DB_PRIVATE flag passed from the application's DB_ENV-\>open method.

- DB_THREAD

  Enables/disables DB_THREAD in the DB_ENV-\>open method. For example:

  ``` c
  set_open_flags DB_THREAD on
  ```

  This enables the DB_ENV handle returned by the DB_ENV-\>open method to be free-threaded; that is, concurrently usable by multiple threads in the address space. This setting overwrites the DB_THREAD flag passed from the application's DB_ENV-\>open method.
