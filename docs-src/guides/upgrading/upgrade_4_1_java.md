---
title: "Java exceptions"
api-name: "Java exceptions"
source: docs/upgrading/upgrade_4_1_java.html
---
## Java exceptions

The Java DbEnv constructor is now marked with "throws DbException". This means applications must construct DbEnv objects in a context where DbException throwables are handled (either in a try/catch block or in a method that propagates the exception up the stack). Note that previous versions of the Berkeley DB Java API could throw this exception from the constructor but it was not marked.
