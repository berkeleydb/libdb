---
title: "Java and C++ object reuse"
api-name: "Java and C++ object reuse"
source: docs/upgrading/upgrade_3_2_handle.html
---
## Java and C++ object reuse

In previous releases of Berkeley DB, Java DbEnv and Db objects, and C++ <a href="../api_reference/CXX/env.html" class="olink">DbEnv</a> and <a href="../api_reference/CXX/db.html" class="olink">Db</a> objects could be reused after they were closed, by calling open on them again. This is no longer permitted, and these objects no longer allow any operations after a close. Applications reusing these objects should be modified to create new objects instead.
