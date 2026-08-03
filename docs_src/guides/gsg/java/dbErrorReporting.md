---
title: "Error Reporting Functions"
api-name: "Error Reporting Functions"
source: docs/gsg/JAVA/dbErrorReporting.html
---
## Error Reporting Functions

To simplify error reporting and handling, the `DatabaseConfig` class offers several useful methods.

- `DatabaseConfig.setErrorStream()`

  Sets the Java `OutputStream` to be used for displaying error messages issued by the DB library.

- `DatabaseConfig.setMessageHandler()`

  Defines the message handler that is called when an error message is issued by DB. The error prefix and message are passed to this callback. It is up to the application to display this information correctly.

  Note that the message handler must be an implementation of the `com.sleepycat.db.MessageHandler` interface.

- `DatabaseConfig.setErrorPrefix()`

  Sets the prefix used for any error messages issued by the DB library.

For example, to send all your error messages to a particular message handler, first implement the handler:

``` c
package db.GettingStarted;

import com.sleepycat.db.Environment;
import com.sleepycat.db.MessageHandler;

public class MyMessageHandler implements MessageHandler  {

    // Our constructor does nothing
    public MyMessageHandler() {}

    public void message(Environment dbenv, String message)
    {
        // Put your special message handling code here
    }

}
```

And then set up your database to use the message handler by identifying it on the database's `DatabaseConfig` object:

``` c
package db.GettingStarted;

import com.sleepycat.db.DatabaseConfig;

...

DatabaseConfig myDbConfig = new DatabaseConfig();
MyMessageHandler mmh = new MyMessageHandler();
myDbConfig.setMessageHandler(mmh); 
```
