---
title: "Database Example"
api-name: "Database Example"
source: docs/gsg/CXX/CoreDbCXXUsage.html
---
## Database Example

Throughout this book we will build a couple of applications that load and retrieve inventory data from DB databases. While we are not yet ready to begin reading from or writing to our databases, we can at least create the class that we will use to manage our databases.

Note that subsequent examples in this book will build on this code to perform the more interesting work of writing to and reading from the databases.

Note that you can find the complete implementation of these functions in:

``` c
DB_INSTALL/examples_cxx/getting_started
```

where <span class="emphasis">*`DB_INSTALL`*</span> is the location where you placed your DB distribution.

**Example 2.1 MyDb Class**

To manage our database open and close activities, we encapsulate them in the `MyDb` class. There are several good reasons to do this, the most important being that we can ensure our databases are closed by putting that activity in the `MyDb` class destructor.

To begin, we create our class definition:

``` c
// File: MyDb.hpp
#include <db_cxx.h>

class MyDb
{
public:
    // Constructor requires a path to the database,
    // and a database name.
    MyDb(std::string &path, std::string &dbName);

    // Our destructor just calls our private close method.
    ~MyDb() { close(); }

    inline Db &getDb() {return db_;}

private:
    Db db_;
    std::string dbFileName_;
    u_int32_t cFlags_;

    // Make sure the default constructor is private
    // We don't want it used.
    MyDb() : db_(NULL, 0) {}

    // We put our database close activity here.
    // This is called from our destructor. In
    // a more complicated example, we might want
    // to make this method public, but a private
    // method is more appropriate for this example.
    void close();
}; 
```

Next we need the implementation for the constructor:

``` c
// File: MyDb.cpp
#include "MyDb.hpp"

// Class constructor. Requires a path to the location
// where the database is located, and a database name
MyDb::MyDb(std::string &path, std::string &dbName)
    : db_(NULL, 0),               // Instantiate Db object
      dbFileName_(path + dbName), // Database file name
      cFlags_(DB_CREATE)          // If the database doesn't yet exist,
                                  // allow it to be created.
{
    try
    {
        // Redirect debugging information to std::cerr
        db_.set_error_stream(&std::cerr);

        // Open the database
        db_.open(NULL, dbFileName_.c_str(), NULL, DB_BTREE, cFlags_, 0);
    }
    // DbException is not a subclass of std::exception, so we
    // need to catch them both.
    catch(DbException &e)
    {
        std::cerr << "Error opening database: " << dbFileName_ << "\n";
        std::cerr << e.what() << std::endl;
    }
    catch(std::exception &e)
    {
        std::cerr << "Error opening database: " << dbFileName_ << "\n";
        std::cerr << e.what() << std::endl;
    }
}
```

And then we need the implementation for the `close()` method:

``` c
// Private member used to close a database. Called from the class
// destructor.
void
MyDb::close()
{
    // Close the db
    try
    {
        db_.close(0);
        std::cout << "Database " << dbFileName_
                  << " is closed." << std::endl;
    }
    catch(DbException &e)
    {
        std::cerr << "Error closing database: " << dbFileName_ << "\n";
        std::cerr << e.what() << std::endl;
    }
    catch(std::exception &e)
    {
        std::cerr << "Error closing database: " << dbFileName_ << "\n";
        std::cerr << e.what() << std::endl;
    }
} 
```
