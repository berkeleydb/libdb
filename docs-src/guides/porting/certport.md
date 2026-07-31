---
title: "Certifying a Port of Berkeley DB"
api-name: "Certifying a Port of Berkeley DB"
source: docs/porting/certport.html
---
## Certifying a Port of Berkeley DB

When the target platform supports using Tcl, the port is considered certified after a successful standard run (`run_std`) of the Test Suite .

When the target platform does not support using Tcl, a port is considered certified if you see successful message reports for each of the tests in `test_micro` and `test_mutex`.

Additionally, the configuration and compilation of Berkeley DB must be complete without errors or warninigs of any kind. You must provide specific information about the hardware, software, and compiler used during the porting process, especially versions of all thrid party tools used. If you have other diagnostic tools available, such as memory allocation checking tools, please conduct tests using them as well and provide Oracle engineering with the results. Finally, do not constrain your testing to one configuration. There are many different ways to configure Berkeley DB (that is many options to the "configure" script), please configure, built, and test Berkeley DB on the target platform with as many combinations of these flags as possible to ensure that nothing is missed.
