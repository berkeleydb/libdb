---
title: "Programmer notes FAQ"
api-name: "Programmer notes FAQ"
source: docs/programmer_reference/program_faq.html
---
## Programmer notes FAQ

1.  **What priorities should threads/tasks executing Berkeley DB functions be given?**

    Tasks executing Berkeley DB functions should have the same, or roughly equivalent, system priorities. For example, it can be dangerous to give tasks of control performing checkpoints a lower priority than tasks of control doing database lookups, and starvation can sometimes result.

2.  **Why isn't the C++ API exception safe?**

    The Berkeley DB C++ API is a thin wrapper around the C API that maps most return values to exceptions, and gives the C++ handles the same lifecycles as their C counterparts. One consequence is that if an exception occurs while a cursor or transaction handle is open, the application must explicitly close the cursor or abort the transaction.

    Applications can be simplified and bugs avoided by creating wrapper classes around <a href="../../api/c/dbc.md" class="olink">DBC</a> and <a href="../../api/c/txn.md" class="olink">TXN</a> that call the appropriate cleanup method in the wrapper's destructor. By creating an instance of the wrappers on the stack, C++ scoping rules will ensure that the destructor is called before exception handling unrolls the block that contains the wrapper object.

3.  **My Linux library was configured with --enable-dtrace and built without error. Why can my tracer not find the `bdb` probes?**

    First check that the probes are in the library: `readelf -n libdb-2026.0.so | grep -c 'Provider: bdb'` must print a number above zero. Then attach by the path of the library the process actually loads, with `-` in a probe name written `__` (`usdt:/path/to/libdb-2026.0.so:bdb:txn__commit`). bpftrace and perf need root, or `CAP_BPF` and `CAP_PERFMON`. See [Performance Event Monitoring](program_perfmon.md).
