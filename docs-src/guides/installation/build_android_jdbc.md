---
title: "Building the Android JDBC Driver"
api-name: "Building the Android JDBC Driver"
source: docs/installation/build_android_jdbc.html
---
## Building the Android JDBC Driver

This section describes how to build and use the BDB JDBC driver for Android. Note that the BDB JDBC driver cannot currently be built on a Windows platform.

1.  Download and install the Android SDK. The installation instructions can be found here:

    <a href="http://developer.android.com/sdk/installing.html" class="ulink" target="_top">http://developer.android.com/sdk/installing.html</a>

2.  Download and install the Android NDK. It can be found here:

    <a href="http://developer.android.com/sdk/ndk/index.html" class="ulink" target="_top">http://developer.android.com/sdk/ndk/index.html</a>

3.  Build the BDB JDBC libraries.

    1.  If you do not already have it, download the Berkeley DB package from here:

        <a href="http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html" class="ulink" target="_top">http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html</a>

        Note that you must be using a 5.3.x or higher version of the product in order for these instructions to work. Once you have the package, unpack it:

        ``` c
        $ tar zxvf db-x.y.z.tar.gz
        $ cd db-x.y.z
        ```

        Where `x.y.z` the major, minor, and point release version of the Berkeley DB distribution which you are using.

        Also, note that in the following instructions, the directory denoted by `db-x.y.z`, above, is referred to as `<db>`.

    2.  Build an x86/x64 JDBC package. This is required because the building process will generate target files which are required to build Android NDK. Also, the built JAR file can be imported by eclipse, which will then convert it to the Android Dalvik JAR format.

        To do this, edit `<db>/lang/sql/jdbc/SQLit/Database.java` and replace all instances of `System.loadLibrary("sqlite_jni")` with `System.loadLibrary("oracle-jdbc")`.

        Once you have done this, configure and make the library. The following example shows the minimum configuration options that you need to use in order to configure the Berkeley DB JDBC driver. For your particular installation, other configuration options might be interesting to you. See <a href="build_unix_conf.md" class="xref" title="Configuring Berkeley DB">Configuring Berkeley DB</a> and <a href="build_android_config.md" class="xref" title="Android Configuration Options">Android Configuration Options</a> for more information.

        ``` c
        cd <db>/build_unix
        ../dist/configure --enable-jdbc && make
        ```

4.  Build the Android NDK:

    ``` c
    $ cd <db>/build_android/jdbc/jni
    $ <ndk-install-directory>/ndk-build 
    ```

    This results in the following required files:

    |                                                          |
    |----------------------------------------------------------|
    | \<db\>/build_android/jdbc/libs/armeabi/liboracle-jdbc.so |
    | \<db\>/build_android/jdbc/libs/armeabi/dbsql             |
    | \<db\>/build_unix/jdbc/sqlite.jar                        |

Having built the JDBC driver, you can now use it with your project. You can do this using Eclipse and the ADT plugin, which you can get from here:

<a href="http://developer.android.com/sdk/eclipse-adt.html" class="ulink" target="_top">http://developer.android.com/sdk/eclipse-adt.html</a>

To make sure everything is working:

1.  Start Eclipse and create an Android project. Use:

    - `test_jdbc` as the Android project name.

    - Create it as an Android 3.2 project.

    - For the package name, use `example.jdbc`.

2.  This results in an empty code file. Copy and paste the following example code into that file:

    ``` c
    package example.testjdbc;

    import SQLite.*;
    import java.io.*;

    import android.app.Activity;
    import android.os.Bundle;
    import android.widget.TextView;
    import java.sql.*;

    public class Test_jdbcActivity extends Activity {

        /*
         * This is the main entrance/body of our sample program. This
         * example illustrates all of the major API usages.
         */
        public void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);

            TextView tv = new TextView(this);
            tv.setText("App Started");
            setContentView(tv);

            System.out.println("Appstart: ");

            String url = 
                "jdbc:sqlite://data/data/example.testjdbc/example.db";
            Connection con;
            String dropString = "drop table if exists COFFEES";
            String createString;
            createString = "create table COFFEES " 
                + "(COF_NAME varchar(32), "
                + "SUP_ID int, " + "PRICE float, " + "SALES int, "
                + "TOTAL int)";
            String insertString = "drop table COFFEES if exisits";
            String query = "select COF_NAME, PRICE from COFFEES";
            Statement stmt;

            try {
                Class.forName("SQLite.JDBCDriver");

            } catch (java.lang.ClassNotFoundException e) {
                System.err.print("ClassNotFoundException: ");
                System.err.println(e.getMessage());
            }

            try {
                con = 
                    DriverManager.getConnection(url, "myLogin", "myPW");

                stmt = con.createStatement();
                stmt.executeUpdate(dropString);
                stmt.executeUpdate(createString);
                stmt.close();

                stmt = con.createStatement();
                stmt.executeUpdate("insert into COFFEES "
                    + "values('Colombian', 00101, 7.99, 0, 0)");

                stmt.executeUpdate("insert into COFFEES "
                    + "values('French_Roast', 00049, 8.99, 0, 0)");

                stmt.executeUpdate("insert into COFFEES "
                    + "values('Espresso', 00150, 9.99, 0, 0)");

                stmt.executeUpdate("insert into COFFEES "
                    + "values('Colombian_Decaf', 00101, 8.99, 0, 0)");

                stmt.executeUpdate("insert into COFFEES "
                    + "values('French_Roast_Decaf', 00049, 9.99, 0, 0)");

                ResultSet rs = stmt.executeQuery(query);

                System.out.println("Coffee Break Coffees and Prices:");
                while (rs.next()) {
                    String s = rs.getString("COF_NAME");
                    float f = rs.getFloat("PRICE");
                    System.out.println(s + "   " + f);
                }
                stmt.close();
                con.close();

            } catch (SQLException ex) {
                System.err.println("SQLException: " + ex.getMessage());
            }
        }
    } 
    ```

3.  Copy the following files into place:

    ``` c
    $ cd <workspace>/test_jdbc
    $ mkdir -p libs/armeabi
    $ cp -r <db>/build_android/jdbc/libs/armeabi/liboracle-jdbc.so \
    libs/armeabi
    $ cp -r <db>/build_unix/jdbc/sqlite.jar libs
    ```

4.  Back in Eclipse, right click the project name, and select the `refresh` option to reload the project from the directory. The two new files that were copied into place in the previous step are now included in the project view.

5.  Convert the JAR file to the Android Dalvik format:

    1.  Right-click on your project.

    2.  Choose `Build Path -> Configure Build Path`

    3.  Click the `Libraries` tab.

    4.  Click `Add JARS`.

6.  Run the project:

    1.  Choose `Property -> Android` and select any one of the usable build targets.

    2.  Right click the project. Choose `Run As -> Android`

7.  Verify your installation. After a short pause (depending on the speed of your system), the application logo is displayed. Use the Android adb command line application to make sure the application is running as expected:

    ``` c
    $ cd <android-sdk>/platform-tools
    $ ./adb logcat
    I/System.out(  539): Appstart: 
    I/System.out(  539): Coffee Break Coffees and Prices:
    I/System.out(  539): Colombian   7.99
    I/System.out(  539): French_Roast   8.99
    I/System.out(  539): Espresso   9.99
    I/System.out(  539): Colombian_Decaf   8.99
    I/System.out(  539): French_Roast_Decaf   9.99
    ```

    You can also check if the database (`example.db`) exists in the emulator:

    ``` c
    $ ./adb shell ls /data/data/example.testjdbc
    example.db
    example.db-journal
    lib 
    ```

    Finally, check the database using the BDB SQL shell:

    ``` c
    $ ./adb push <db>/build_android/jdbc/libs/armeabi/dbsql \
    /data/data/example.testjdbc
    326 KB/s (1293760 bytes in 3.865s)
    $ ./adb shell
    root@android:/ # cd /data/data/example.testjdbc
    root@android:/data/data/example.testjdbc # ./dbsql example.db
    Berkeley DB 11g Release 2, library version 11.2.5.2.36
    Enter ".help" for instructions
    Enter SQL statements terminated with a ";"
    dbsql> .tables
    COFFEES
    dbsql> select * from COFFEES;
    Colombian|101|7.99|0|0
    French_Roast|49|8.99|0|0
    Espresso|150|9.99|0|0
    Colombian_Decaf|101|8.99|0|0
    French_Roast_Decaf|49|9.99|0|0
    dbsql> .quit 
    ```
