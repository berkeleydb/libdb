/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 2010 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include <sys/types.h>

#include <iostream>
#include <iomanip>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
extern "C" {
	extern int getopt(int, char * const *, const char *);
	extern int optind;
}
#else
#include <unistd.h>
#endif

#include <db_cxx.h>

#define	DATABASE	"access.db"

using std::cin;
using std::cout;
using std::cerr;

class ResMgmtExample
{
public:
	ResMgmtExample();
	void run();
	void run_c();
	void run_heap();

private:
	int open_db(Db &db, const char *fileName);
	int scan_db(DbEnv &dbenv, Db &db);
	void input(DbEnv &dbenv, Db &db1, Db&db2, Db&db3, Db&db4);

	int open_db(DB_ENV *dbenv, DB **pdb, const char *fileName);
	int scan_db(DB_ENV *dbenv, DB *db);
	void input(DB_ENV *dbenv, DB *db1, DB *db2, DB *db3, DB *db4);

	// no need for copy and assignment
	ResMgmtExample(const ResMgmtExample &);
	void operator = (const ResMgmtExample &);
};

int
usage()
{
	(void)fprintf(stderr, "usage: ResMgmtExample [-r] [database]\n");
	return (EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
	int ch, rflag;
	const char *database;

	rflag = 0;
	while ((ch = getopt(argc, argv, "r")) != EOF)
		switch (ch) {
		case 'r':
			rflag = 1;
			break;
		case '?':
		default:
			return (usage());
		}
	argc -= optind;
	argv += optind;

	/* Accept optional database name. */
	database = *argv == NULL ? DATABASE : argv[0];

	// Use a try block just to report any errors.
	// An alternate approach to using exceptions is to
	// use error models (see DbEnv::set_error_model()) so
	// that error codes are returned for all Berkeley DB methods.
	//
	try {
		ResMgmtExample app;
		cout<<"\nRunning on stack... \n";
		app.run();
		cout<<"\nRunning on heap... \n";
		app.run_heap();

		cout<<"\nRunning C version ... \n";
		app.run_c();
		return (EXIT_SUCCESS);
	}
	catch (DbException &dbe) {
		cerr << "ResMgmtExample: " << dbe.what() << "\n";
		return (EXIT_FAILURE);
	}
}

ResMgmtExample::ResMgmtExample()
{
}

int ResMgmtExample::open_db(DB_ENV *dbenv, DB **pdb, const char *fileName)
{
	DB *db;
	int ret;

	db_create(&db, dbenv, 0);
	db->set_errpfx(db, "ResMgmtExample");
	db->set_pagesize(db, 1024);		/* Page size: 1K. */
	ret = db->open(db, NULL, fileName, NULL, DB_BTREE, 
	    DB_CREATE | DB_AUTO_COMMIT, 0664);
	if (ret == 0)
		*pdb = db;

	return ret;

}

int ResMgmtExample::open_db(Db &db, const char *fileName)
{
	db.set_error_stream(&cerr);
	db.set_errpfx("ResMgmtExample");
	db.set_pagesize(1024);		/* Page size: 1K. */
	return db.open(NULL, fileName, NULL, DB_BTREE, 
	    DB_CREATE | DB_AUTO_COMMIT, 0664);

}

void ResMgmtExample::run()
{

	DbEnv dbenv(0);
	dbenv.open("TESTDIR", DB_INIT_TXN | DB_INIT_MPOOL | DB_INIT_LOG | 
	    DB_INIT_LOCK | DB_CREATE, 0666);

	Db db1(&dbenv, 0);
	open_db(db1, "db1.db");
	Db db2(&dbenv, 0);
	open_db(db2, "db2.db");
	Db db3(&dbenv, 0);
	open_db(db3, "db3.db");
	Db db4(&dbenv, 0);
	open_db(db4, "db4.db");


	input(dbenv, db1, db2, db3, db4);	

	cout << "\n";
	scan_db(dbenv, db1);
	scan_db(dbenv, db2);
	scan_db(dbenv, db3);
	scan_db(dbenv, db4);

}

void ResMgmtExample::run_c()
{
	DB_ENV *dbenv;
	DB *db1, *db2, *db3, *db4;

	db_env_create(&dbenv, 0);
	dbenv->open(dbenv, "TESTDIR", DB_INIT_TXN | DB_INIT_MPOOL | 
	    DB_INIT_LOG | DB_INIT_LOCK | DB_CREATE, 0666);

	open_db(dbenv, &db1, "db1.db");
	open_db(dbenv, &db2, "db2.db");
	open_db(dbenv, &db3, "db3.db");
	open_db(dbenv, &db4, "db4.db");


	input(dbenv, db1, db2, db3, db4);	

	cout << "\n";
	scan_db(dbenv, db1);
	scan_db(dbenv, db2);
	scan_db(dbenv, db3);
	scan_db(dbenv, db4);

	dbenv->close(dbenv, DB_FORCESYNC);

}

void ResMgmtExample::run_heap()
{

	DbEnv *dbenv = new DbEnv(0);
	dbenv->open("TESTDIR", DB_INIT_TXN | DB_INIT_MPOOL | DB_INIT_LOG | 
	    DB_INIT_LOCK | DB_CREATE, 0666);

	Db *db1 = new Db(dbenv, 0);
	open_db(*db1, "db1.db");
	Db *db2 = new Db(dbenv, 0);
	open_db(*db2, "db2.db");
	Db *db3 = new Db(dbenv, 0);
	open_db(*db3, "db3.db");
	Db *db4 = new Db(dbenv, 0);
	open_db(*db4, "db4.db");


	input(*dbenv, *db1, *db2, *db3, *db4);	

	cout << "\n";
	scan_db(*dbenv, *db1);
	scan_db(*dbenv, *db2);
	scan_db(*dbenv, *db3);
	scan_db(*dbenv, *db4);

	// The delete order should be irrelevant, dbenv can be deleted 
	// before or after databases.
	delete dbenv;
	delete db3;
	delete db1;
	delete db2;
	delete db4;
}

void ResMgmtExample::input(DbEnv &dbenv, Db &db1, Db&db2, Db&db3, Db&db4)
{

	//
	// Insert records into the database, where the key is the user
	// input and the data is the user input in reverse order.
	//
	char buf[1024], rbuf[1024];
	char *p, *t;
	int ret;
	u_int32_t len;
	Dbc *csr1, *csr2, *csr3, *csr4;
	DbTxn *txn;

	dbenv.txn_begin(NULL, &txn, 0);
	db1.cursor(txn, &csr1, 0);
	db2.cursor(txn, &csr2, 0);
	db3.cursor(txn, &csr3, 0);
	db4.cursor(txn, &csr4, 0);

	// Manipulate the databases.
	for (;;) {
		cout << "input> ";
		cout.flush();

		cin.getline(buf, sizeof(buf));
		if (cin.eof())
			break;

		if ((len = (u_int32_t)strlen(buf)) <= 0)
			continue;
		for (t = rbuf, p = buf + (len - 1); p >= buf;)
			*t++ = *p--;
		*t++ = '\0';
		if (strcmp(buf, "quit") == 0)
			break;
		Dbt key(buf, len + 1);
		Dbt data(rbuf, len + 1);

		ret = csr1->put(&key, &data, DB_KEYLAST);
		ret = csr2->put(&key, &data, DB_KEYLAST);
		ret = csr3->put(&key, &data, DB_KEYLAST);
		ret = csr4->put(&key, &data, DB_KEYLAST);
	}
	txn->commit(0);
}

void ResMgmtExample::input(DB_ENV *dbenv, DB *db1, DB *db2, DB *db3, DB *db4)
{
	DBT key, data;
	//
	// Insert records into the database, where the key is the user
	// input and the data is the user input in reverse order.
	//
	char buf[1024], rbuf[1024];
	char *p, *t;
	int ret;
	u_int32_t len;
	DBC *csr1, *csr2, *csr3, *csr4;
	DB_TXN *txn;

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));

	dbenv->txn_begin(dbenv, NULL, &txn, 0);
	db1->cursor(db1, txn, &csr1, 0);
	db2->cursor(db2, txn, &csr2, 0);
	db3->cursor(db3, txn, &csr3, 0);
	db4->cursor(db4, txn, &csr4, 0);

	// Manipulate the databases.
	for (;;) {
		cout << "input> ";
		cout.flush();

		cin.getline(buf, sizeof(buf));
		if (cin.eof())
			break;

		if ((len = (u_int32_t)strlen(buf)) <= 0)
			continue;
		for (t = rbuf, p = buf + (len - 1); p >= buf;)
			*t++ = *p--;
		*t++ = '\0';
		if (strcmp(buf, "quit") == 0)
			break;
		key.data = buf;
		key.size = len + 1;
		data.data = rbuf;
		data.size = len + 1;

		ret = csr1->put(csr1, &key, &data, DB_KEYLAST);
		ret = csr2->put(csr2, &key, &data, DB_KEYLAST);
		ret = csr3->put(csr3, &key, &data, DB_KEYLAST);
		ret = csr4->put(csr4, &key, &data, DB_KEYLAST);
	}
	txn->commit(txn, 0);
}

int ResMgmtExample::scan_db(DbEnv &dbenv, Db &db)
{

	// We put a try block around this section of code
	// to ensure that our database is properly closed
	// in the event of an error.
	//
	try {
		// Acquire a cursor for the table.
		Dbc *dbcp;
		DbTxn *txn;
		dbenv.txn_begin(NULL, &txn, 0);
		db.cursor(txn, &dbcp, 0);

		// Walk through the table, printing the key/data pairs.
		Dbt key;
		Dbt data;
		while (dbcp->get(&key, &data, DB_NEXT) == 0) {
			char *key_string = (char *)key.get_data();
			char *data_string = (char *)data.get_data();
			cout << key_string << " : " << data_string << "\n";
		}
		txn->commit(0);
	}
	catch (DbException &dbe) {
		cerr << "ResMgmtExample: " << dbe.what() << "\n";
	}
}


int ResMgmtExample::scan_db(DB_ENV *dbenv, DB *db)
{

	// We put a try block around this section of code
	// to ensure that our database is properly closed
	// in the event of an error.
	//
	// Acquire a cursor for the table.
	DBC *dbcp;
	DB_TXN *txn;
	DBT key, data;

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));
	dbenv->txn_begin(dbenv, NULL, &txn, 0);
	db->cursor(db, txn, &dbcp, 0);

	// Walk through the table, printing the key/data pairs.
	while (dbcp->get(dbcp, &key, &data, DB_NEXT) == 0) {
		char *key_string = (char *)key.data;
		char *data_string = (char *)data.data;
		cout << key_string << " : " << data_string << "\n";
	}
	txn->commit(txn, 0);
}
