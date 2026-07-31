---
title: "db_sql_codegen"
api-name: "db_sql_codegen"
source: docs/api_reference/C/db_sql_codegen.html
---
## db_sql_codegen

``` c
db_sql_codegen [-i <ddl input file>] [-o <output C code file>] 
    [-h <output header file>] [-t <test output file>]  
```

<span class="command">**Db_sql_codegen**</span> is a utility program that translates a schema description written in a SQL Data Definition Language dialect into C code that implements the schema using Berkeley DB. It is intended to provide a quick and easy means of getting started with Berkeley DB for users who are already conversant with SQL. It also introduces a convenient way to express a Berkeley DB schema in a format that is both external to the program that uses it and compatible with relational databases.

The <span class="command">**db_sql_codegen**</span> command reads DDL from an input stream, and writes C code to an output stream. With no command line options, it will read from stdin and write to stdout. A more common usage mode would be to supply the DDL in a named input file (-i option). With only the -i option, <span class="command">**db_sql_codegen**</span> will produce two files: a C-language source code (.c) file and a C-language header (.h) file, with names that are derived from the name of the input file. You can also control the names of these output files with the -o and -h options. The -x option causes the generated code to be transaction-aware. Finally, the -t option will produce a simple application that invokes the generated function API. This is a C-language source file that includes a main function, and serves the dual purposes of providing a simple test for the generated C code, and of being an example of how to use the generated API.

The options are as follows:

- **-i**\<ddl input file\>

  Names the input file containing SQL DDL.

- **-o** \<output C code file\>

  Names the output C-language source code file.

- **-h** \<output header file\>

  Names the output C-language header file.

- **-t** \<test output file\>

  Names the output C-langage test file.

- **-x**

  Sets the default transaction mode to TRANSACTIONAL.

The <span class="command">**db_sql_codegen**</span> utility exits 0 on success, and \>0 if an error occurs.

Note that the <span class="command">**db_sql_codegen**</span> utility is built only when --enable-sql_codegen option is passed as an argument when you are configuring Berkeley DB. For more information, see <a href="../../installation/build_unix_conf.html" class="olink">"Configuring Berkeley DB"</a>

### Input Syntax

The input file can contain the following SQL DDL statements.

- **CREATE DATABASE**

  The DDL must contain a CREATE DATABASE statement. The syntax is simply

  ``` c
  CREATE DATABASE name;
  ```

  . The name given here is used as the name of the Berkeley DB environment in which the Berkeley DB databases are created.

- **CREATE TABLE**

  Each CREATE TABLE statement produces functions to create and delete a primary Berkeley DB database. Also produced are functions to perform record insertion, retrieval and deletion on this database.

  CREATE TABLE establishes the field set of records that can be stored in the Berkeley DB database. Every CREATE TABLE statement must identify a primary key to be used as the lookup key in the Berkeley DB database.

  Here is an example to illustrate the syntax of CREATE TABLE that is accepted by <span class="command">**db_sql_codegen**</span>:

  ``` c
  CREATE TABLE person (person_id INTEGER PRIMARY KEY,
                          name VARCHAR(64),
                          age INTEGER);
  ```

  This results in the creation of functions to manage a database in which every record is an instance of the following C language data structure:

  ``` c
  typedef struct _person_data {
      int person_id;
      char name[PERSON_DATA_NAME_LENGTH];
      int age;
  } person_data; 
  ```

- **CREATE INDEX** You can create secondary Berkeley DB databases to be used as indexes into a primary database. For example, to make an index on the "name" field of the "person" table mentioned above, the SQL DDL would be:

  ``` c
  CREATE INDEX name_index ON person(name);
  ```

  This causes <span class="command">**db_sql_codegen**</span> to emit functions to manage creation and deletion of a secondary database called "name_index," which is associated with the "person" database and is set up to perform lookups on the "name" field.

### Hint Comments

The SQL DDL input may contain comments. Two types of comments are recognized. C-style comments begin with "/\*" and end with "\*/". These comments may extend over multiple lines.

Single line comments begin with "--" and run to the end of the line.

If the first character of a comment is "+" then the comment is interpreted as a "hint comment." Hint comments can be used to configure Berkeley DB features that cannot be represented in SQL DDL.

Hint comments are comma-separated lists of property assignments of the form "property=value." Hint comments apply to the SQL DDL statement that immediately precedes their appearance in the input. For example:

``` c
CREATE DATABASE peopledb; /*+ CACHESIZE = 16m */
```

This causes the generated environment creation function to set the cache size to sixteen megabytes.

In addition to the CACHESIZE example above, two other hint comment keywords are recognized: DBTYPE and MODE.

After a CREATE TABLE or CREATE INDEX statement, you may set the database type by assigning the DBTYPE property in a hint comment. Possible values for DBTYPE are BTREE and HASH.

After a CREATE DATABASE or CREATE TABLE statement, you may tell <span class="command">**db_sql_codegen**</span> whether to generate transaction-aware code by assigning the MODE property in a hint comment. The possible values for MODE are TRANSACTIONAL and NONTRANSACTIONAL. By default, generated code is not transaction-aware. If MODE=TRANSACTIONAL appears on a CREATE DATABASE statement, then the default for every CREATE TABLE statement becomes TRANSACTIONAL. Individual CREATE TABLE statements may have MODE=TRANSACTIONAL or MODE=NONTRANSACTIONAL, to control whether the code generated for accessing and updating the associated Berkeley DB database is transaction aware.

### Transactions

By default, the code generated by <span class="command">**db_sql_codegen**</span> is not transaction-aware. This means that the generated API for reading and updating BDB databases operates in nontransactional mode. When transactional mode is enabled, either through the command-line option **-x** or by the inclusion of MODE-setting hint comments in the DDL source, the generated data access functions take an extra argument which is a pointer to DB_TXN. To use transactions, application code must acquire a DB_TXN from a call to DB_ENV-\>txn_begin, and supply a pointer to this object when invoking the db_sql_codegen-generated functions that requre such an argument.

Transaction-aware APIs that were generated by db_sql_codegen can be used in nontransactional mode by passing NULL for the DB_TXN pointer arguments.

For more information about using BDB transactions, please consult the documentation for <a href="txn.md#txnlist" class="xref" title="Transaction Subsystem and Related Methods">Transaction Subsystem and Related Methods</a> .

### Type Mapping

<span class="command">**db_sql_codegen**</span> must map the schema expressed as SQL types into C language types. It implements the following mappings:

``` c
BIN     char[]
VARBIN      char[]
CHAR        char[]
VARCHAR     char[]
VARCHAR2    char[]
BIT         char
TINYINT     char
SMALLINT    short
INTEGER     int
INT         int
BIGINT      long
REAL        float
DOUBLE      double
FLOAT       double
DECIMAL     double
NUMERIC     double
NUMBER(p,s) int, long, float, or double 
```

While BIN/VARBIN and CHAR/VARCHAR are both represented as char arrays, the latter are treated as null-terminated C strings, while the former are treated as binary data.

The Oracle type NUMBER is mapped to different C types, depending on its precision and scale values. If scale is 0, then it is mapped to an integer type (long if precision is greater than 9). Otherwise it is mapped to a floating point type (float if precision is less than 7, otherwise double).

### Output

Depending on the options given on the command line, <span class="command">**db_sql_codegen**</span> can produce three separate files: a .c file containing function definitions that implement the generated API; a .h file containing constants, data structures and prototypes of the generated functions; and a second .c file that contains a sample program that invokes the generated API. The latter program is usually referred to as a smoke test.

Given the following sample input in a file named "people.sql":

``` c
CREATE DATABASE peopledb;
CREATE TABLE person (person_id INTEGER PRIMARY KEY,
                  name VARCHAR(64),
                  age INTEGER);
CREATE INDEX name_index ON person(name);
```

The command

``` c
db_sql_codegen -i people.sql -t test_people.c
```

Will produce files named people.h, people.c, and test_people.c.

The file people.h will contain the information needed to use the generated API. Among other things, an examination of the generated .h file will reveal:

``` c
#define PERSON_DATA_NAME_LENGTH 63
```

This is just a constant for the length of the string mapped from the VARCHAR field.

``` c
typedef struct _person_data {
   int   person_id;
   char  name[PERSON_DATA_NAME_LENGTH];
   int   age;
} person_data; 
```

This is the data structure that represents the record type that is stored in the person database. There's that constant being used.

``` c
int create_peopledb_env(DB_ENV **envpp);
int create_person_database(DB_ENV *envp, DB **dbpp);
int create_name_index_secondary(DB_ENV *envp, DB *primary_dbp,
                            DB **secondary_dbpp); 
```

These functions must be invoked to initialize the Berkeley DB environment. However, see the next bit:

``` c
extern DB_ENV * peopledb_envp;
extern DB *person_dbp;
extern DB *name_index_dbp;

int initialize_peopledb_environment(); 
```

For convenience, <span class="command">**db_sql_codegen**</span> provides global variables for the environment and database, and a single initialization function that sets up the environment for you. You may choose to use the globals and the single initialization function, or you may declare your own DB_ENV and DB pointers, and invoke the individual create\_\* functions yourself.

The word "create" in these function names might be confusing. It means "create the environment/database if it doesn't already exist; otherwise open it."

All of the functions in the generated API return Berkeley DB error codes. If the return value is non-zero, there was an error of some kind, and an explanatory message should have been printed on stderr.

``` c
int person_insert_struct(DB *dbp, person_data *personp);
int person_insert_fields(DB * dbp,
        int person_id,
        char *name,
        int age); 
```

These are the functions that you'd use to store a record in the database. The first form takes a pointer to the data structure that represents this record. The second form takes each field as a separate argument.

If two records with the same primary key value are stored, the first one is lost.

``` c
int get_person_data(DB *dbp, int person_key, person_data *data);
```

This function retrieves a record from the database. It seeks the record with the supplied key, and populates the supplied structure with the contents of the record. If no matching record is found, the function returns DB_NOTFOUND.

``` c
int delete_person_key(DB *dbp, int person_key);
```

This function removes the record matching the given key.

``` c
typedef void (*person_iteration_callback)(void *user_data, 
                                         person_data *personp);

int person_full_iteration(DB *dbp,
    person_iteration_callback user_func,
    void *user_data); 
```

This function performs a complete iteration over every record in the person table. The user must provide a callback function which is invoked once for every record found. The user's callback function must match the prototype provided in the typedef "person_iteration_callback." In the callback, the "user_data" argument is passed unchanged from the "user_data" argument given to person_full_iteration. This is provided so that the caller of person_full_iteration can communicate some context information to the callback function. The "personp" argument to the callback is a pointer to the record that was retrieved from the database. Personp points to data that is valid only for the duration of the callback invocation.

``` c
int name_index_query_iteration(DB *secondary_dbp,
    char *name_index_key,
    person_iteration_callback user_func,
    void *user_data); 
```

This function performs lookups through the secondary index database. Because duplicate keys are allowed in secondary indexes, this query might return multiple instances. This function takes as an argument a pointer to a user-written callback function, which must match the function prototype typedef mentioned above (person_iteration_callback). The callback is invoked once for each record that matches the secondary key.

### Test output

The test output file is useful as an example of how to invoke the generated API. It will contain calls to the functions mentioned above, to store a single record and retrieve it by primary key and through the secondary index.

To compile the test, you would issue a command such as

``` c
 cc -I$BDB_INSTALL/include -L$BDB_INSTALL/lib -o test_people people.c \
     test_people.c -ldb-4.8
```

This will produce the executable file test_people, which can be run to exercise the generated API. The program generated from people.sql will create a database environment in a directory named "peopledb." This directory must be created before the program is run.
