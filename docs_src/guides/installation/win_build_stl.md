---
title: "Building the C++ STL API"
api-name: "Building the C++ STL API"
source: docs/installation/win_build_stl.html
---
## Building the C++ STL API

In the project list of the `Berkeley_DB.sln ` solution, build the "db_stl" project and "db_stl_static" project to build STL API as a dynamic or static library respectively. And in your application, you should link this library file as well as the Berkeley DB library file to your application. The STL API library file is by default always put at the same location as the Berkeley DB library file.

And you need to include the STL API header files in your application code. If you are using the Berkeley DB source tree, the header files are in \<Berkeley DB Source Root \>/stl directory; If you are using the pre-built installed version, these header files are in \< Berkeley DB Installed Directory\>/include, as well as the db.h and db_cxx.h header files.
