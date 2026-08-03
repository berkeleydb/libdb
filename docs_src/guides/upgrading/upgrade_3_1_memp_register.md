---
title: "memp_register"
api-name: "memp_register"
source: docs/upgrading/upgrade_3_1_memp_register.html
---
## memp_register

An additional argument has been added to the **pgin** and **pgout** functions provided to the memp_register function. The application should be searched for any occurrences of memp_register. For each one, if **pgin** or **pgout** functions are specified, the **pgin** and **pgout** functions should be modified to take an initial argument of a **DB_ENV \***. This argument is intended to support better error reporting for applications, and may be entirely ignored by the **pgin** and **pgout** functions themselves.
