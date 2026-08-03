---
title: "Sizing a database environment"
api-name: "Sizing a database environment"
source: docs/programmer_reference/env_size.html
---
## Sizing a database environment

The Berkeley DB environment allocates memory to hold shared structures, either in shared regions or in process data space (if the <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> flag is specified). There are three distinct memory regions:

- The memory pool (also known as the database page cache),

- the area containing mutexes, and

- the main region which holds all other shared structures.

The shared structures in the main region are used by the lock, transaction, logging, thread and replicatoin subsystems.

Determining the amount of space allocated for each of these shared structures is dependent upon the structure in question. The sizing of the memory pool is discussed in <a href="mp_config.md" class="xref" title="Configuring the memory pool">Configuring the memory pool</a>. The amount of memory needed for mutexes is calculated from the number of mutexes needed by various subsystems and can be adjusted using the <a href="../../api/c/mutexset_increment.md" class="olink">DB_ENV-&gt;mutex_set_increment()</a> method.

For applications using shared memory (that is, they do not specify <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a>), a maximum memory size for the main region must be specified or left to default. The maximum memory size is specified using the <a href="../../api/c/envset_memory_max.md" class="olink">DB_ENV-&gt;set_memory_max()</a> method.

The amount of memory needed by an application is dependent on the resources that the application uses. For a very rough estimate, add all of the following together:

1.  The environment has an overhead of about 80 kilobytes without statistics enabled or 250 kilobytes with statistics enabled.

2.  Identify the amount of space you require for your locks:

    1.  Estimate the number of threads of control that will simultaneously access the environment.

    2.  Estimate the number of concurrency locks that, on average, will be required by each thread. For information on sizing concurrency locks, see <a href="lock_max.md" class="xref" title="Configuring locking: sizing the system">Configuring locking: sizing the system</a>.

    3.  Multiply these two numbers, then multiply by 1/2 to arrive at the number of kilobytes required to service your locks.

3.  Estimate the number of open database handles you will use at any given time. For each database handle, there is an overhead of about 1/2 kilobyte.

4.  Add 1 kilobyte for each active transaction.

Note that these are very rough guidelines. It is best to overestimate the needs of your applications, because if the memory allocation is exhausted the application must be shutdown to increase the allocation.

The estimate for maximum memory need not be exact. In most situations there is little penalty for over estimating. For systems using memory mapped files for the shared environment, this only allocates the address space in the process to hold the maximum memory. The backing file will only be extended as needed. For systems running with <a href="../../api/c/envopen.md#envopen_DB_PRIVATE" class="olink">DB_PRIVATE</a> specified, the maximum memory serves only as a limit and memory is allocated from the process data space as needed. No maximum need be set for private environments.

For locking and thread information, groups of objects are allocated when needed so that there is less contention in the allocator during performance critical operations. Once allocated to a particular use, this memory will only be used for that structure. To avoid runtime contention, or to ensure a minimum number of a particular type of object, the <a href="../../api/c/envset_memory_init.md" class="olink">DB_ENV-&gt;set_memory_init()</a> method can be used. This method can set the initial numbers of particular types of structures to allocate at environment creation time.
