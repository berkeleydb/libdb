/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2001,2008 Oracle.  All rights reserved.
 *
 * $Id: RepQuoteExample.cpp,v 1.21 2008/04/28 02:59:56 alexg Exp $
 */

/*
 * In this application, we specify all communication via the command line.  In
 * a real application, we would expect that information about the other sites
 * in the system would be maintained in some sort of configuration file.  The
 * critical part of this interface is that we assume at startup that we can
 * find out
 * 	1) what host/port we wish to listen on for connections,
 * 	2) a (possibly empty) list of other sites we should attempt to connect
 * 	to; and
 * 	3) what our Berkeley DB home environment is.
 *
 * These pieces of information are expressed by the following flags.
 * -m host:port (required; m stands for me)
 * -o host:port (optional; o stands for other; any number of these may be
 *	specified)
 * -h home directory
 * -n nsites (optional; number of sites in replication group; defaults to 0
 *	in which case we try to dynamically compute the number of sites in
 *	the replication group)
 * -p priority (optional: defaults to 100)
 */

#include <iostream>
#include <string>
#include <sstream>

#include <db_cxx.h>
#include "RepConfigInfo.h"
#include "dbc_auto.h"

using std::cout;
using std::cin;
using std::cerr;
using std::endl;
using std::flush;
using std::istream;
using std::istringstream;
using std::string;
using std::getline;

#define	CACHESIZE	(10 * 1024 * 1024)
#define	DATABASE	"quote.db"

const char *progname = "excxx_repquote";

#include <errno.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

extern "C" {
  extern int getopt(int, char * const *, const char *);
  extern char *optarg;
}
#endif

// Struct used to store information in Db app_private field.
typedef struct {
	bool is_master;
} APP_DATA;

static void log(char *);

class RepQuoteExample {
public:
	RepQuoteExample();
	void init(RepConfigInfo* config);
	void doloop();
	int terminate();

	static void event_callback(DbEnv * dbenv, u_int32_t which, void *info);

private:
	// disable copy constructor.
	RepQuoteExample(const RepQuoteExample &);
	void operator = (const RepQuoteExample &);

	// internal data members.
	APP_DATA		app_data;
	RepConfigInfo   *app_config;
	DbEnv		   cur_env;

	// private methods.
	void print_stocks(Db *dbp);
	void prompt();
};

class DbHolder {
public:
	DbHolder(DbEnv *env) : env(env) {
	dbp = 0;
	}

	~DbHolder() {
	try {
		close();
	} catch (...) {
		// Ignore: this may mean another exception is pending
	}
	}

	bool ensure_open(bool creating) {
	if (dbp)
		return (true);
	dbp = new Db(env, 0);
	dbp->set_pagesize(512);

	u_int32_t flags = DB_AUTO_COMMIT;
	if (creating)
		flags |= DB_CREATE;
	try {
		dbp->open(NULL, DATABASE, NULL, DB_BTREE, flags, 0);
		return (true);
	} catch (DbDeadlockException e) {
	} catch (DbRepHandleDeadException e) {
	} catch (DbException e) {
		if (e.get_errno() == DB_REP_LOCKOUT) {
		// Just fall through.
		} else if (e.get_errno() == ENOENT && !creating) {
		// Provide a bit of extra explanation.
		// 
		log("Stock DB does not yet exist");
		} else
		throw;
	}

	// (All retryable errors fall through to here.)
	// 
	log("please retry the operation");
	close();
	return (false);
	}

	void close() {
	if (dbp) {
		try {
		dbp->close(0);
		delete dbp;
		dbp = 0;
		} catch (...) {
		delete dbp;
		dbp = 0;
		throw;
		}
	}
	}

	operator Db *() {
	return dbp;
	}

	Db *operator->() {
	return dbp;
	}

private:
	Db *dbp;
	DbEnv *env;
};

class StringDbt : public Dbt {
public:
#define GET_STRING_OK 0
#define GET_STRING_INVALID_PARAM 1
#define GET_STRING_SMALL_BUFFER 2
#define GET_STRING_EMPTY_DATA 3
	int get_string(char **buf, size_t buf_len)
	{
		size_t copy_len;
		int ret = GET_STRING_OK;
		if (buf == NULL) {
			cerr << "Invalid input buffer to get_string" << endl;
			return GET_STRING_INVALID_PARAM;
		}

		// make sure the string is null terminated.
		memset(*buf, 0, buf_len);

		// if there is no string, just return.
		if (get_data() == NULL || get_size() == 0)
			return GET_STRING_OK;

		if (get_size() >= buf_len) {
			ret = GET_STRING_SMALL_BUFFER;
			copy_len = buf_len - 1; // save room for a terminator.
		} else
			copy_len = get_size();
		memcpy(*buf, get_data(), copy_len);

		return ret;
	}
	size_t get_string_length()
	{
		if (get_size() == 0)
			return 0;
		return strlen((char *)get_data());
	}
	void set_string(char *string)
	{
		set_data(string);
		set_size((u_int32_t)strlen(string));
	}

	StringDbt(char *string) : 
	    Dbt(string, (u_int32_t)strlen(string)) {};
	StringDbt() : Dbt() {};
	~StringDbt() {};

	// Don't add extra data to this sub-class since we want it to remain
	// compatible with Dbt objects created internally by Berkeley DB.
};

RepQuoteExample::RepQuoteExample() : app_config(0), cur_env(0) {
	app_data.is_master = 0; // assume I start out as client
}

void RepQuoteExample::init(RepConfigInfo *config) {
	app_config = config;

	cur_env.set_app_private(&app_data);
	cur_env.set_errfile(stderr);
	cur_env.set_errpfx(progname);
	cur_env.set_event_notify(event_callback);
	cur_env.repmgr_set_ack_policy(DB_REPMGR_ACKS_ALL);

	cur_env.repmgr_set_local_site(app_config->this_host.host,
	    app_config->this_host.port, 0);

	for ( REP_HOST_INFO *cur = app_config->other_hosts; cur != NULL;
		cur = cur->next) {
		cur_env.repmgr_add_remote_site(cur->host, cur->port,
		    NULL, cur->peer ? DB_REPMGR_PEER : 0);
	}

	if (app_config->totalsites > 0)
		cur_env.rep_set_nsites(app_config->totalsites);

	cur_env.rep_set_priority(app_config->priority);

	/*
	 * We can now open our environment, although we're not ready to
	 * begin replicating.  However, we want to have a dbenv around
	 * so that we can send it into any of our message handlers.
	 */
	cur_env.set_cachesize(0, CACHESIZE, 0);
	cur_env.set_flags(DB_TXN_NOSYNC, 1);

	cur_env.open(app_config->home, DB_CREATE | DB_RECOVER |
	    DB_THREAD | DB_INIT_REP | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN, 0);

	if (app_config->verbose)
		cur_env.set_verbose(DB_VERB_REPLICATION, 1);

	cur_env.repmgr_start(3, app_config->start_policy);
}

int RepQuoteExample::terminate() {
	try {
		/*
		 * We have used the DB_TXN_NOSYNC environment flag for
		 * improved performance without the usual sacrifice of
		 * transactional durability, as discussed in the
		 * "Transactional guarantees" page of the Reference
		 * Guide: if one replication site crashes, we can
		 * expect the data to exist at another site.  However,
		 * in case we shut down all sites gracefully, we push
		 * out the end of the log here so that the most
		 * recent transactions don't mysteriously disappear.
				 */
		cur_env.log_flush(NULL);

		cur_env.close(0);
	} catch (DbException dbe) {
		cout << "error closing environment: " << dbe.what() << endl;
	}
	return 0;
}

void RepQuoteExample::prompt() {
	cout << "QUOTESERVER";
	if (!app_data.is_master)
		cout << "(read-only)";
	cout << "> " << flush;
}

void log(char *msg) {
	cerr << msg << endl;
}

// Simple command-line user interface:
//  - enter "<stock symbol> <price>" to insert or update a record in the
//	database;
//  - just press Return (i.e., blank input line) to print out the contents of
//	the database;
//  - enter "quit" or "exit" to quit.
//
void RepQuoteExample::doloop() {
	DbHolder dbh(&cur_env);

	string input;
	while (prompt(), getline(cin, input)) {
		istringstream is(input);
		string token1, token2;

		// Read 0, 1 or 2 tokens from the input.
		//
		int count = 0;
		if (is >> token1) {
			count++;
			if (is >> token2)
			count++;
		}

		if (count == 1) {
			if (token1 == "exit" || token1 == "quit")
			break;
			else {
			log("Format: <stock> <price>");
			continue;
			}
		}

		// Here we know count is either 0 or 2, so we're about to try a
		// DB operation.
		//
		if (!dbh.ensure_open(app_data.is_master))
			continue;

		try {
			if (count == 0)
				print_stocks(dbh);
			else if (!app_data.is_master)
				log("Can't update at client");
			else {
				const char *symbol = token1.c_str();
				StringDbt key(const_cast<char*>(symbol));

				const char *price = token2.c_str();
				StringDbt data(const_cast<char*>(price));

				dbh->put(NULL, &key, &data, 0);
			}
		} catch (DbDeadlockException e) {
			log("please retry the operation");
			dbh.close();
		} catch (DbRepHandleDeadException e) {
			log("please retry the operation");
			dbh.close();
		} catch (DbException e) {
			if (e.get_errno() == DB_REP_LOCKOUT) {
			log("please retry the operation");
			dbh.close();
			} else
			throw;
		}
	}

	dbh.close();
}

void RepQuoteExample::event_callback(DbEnv* dbenv, u_int32_t which, void *info)
{
	APP_DATA *app = (APP_DATA*)dbenv->get_app_private();

	info = NULL;		/* Currently unused. */

	switch (which) {
	case DB_EVENT_REP_MASTER:
		app->is_master = 1;
		break;

	case DB_EVENT_REP_CLIENT:
		app->is_master = 0;
		break;

	case DB_EVENT_REP_STARTUPDONE: /* FALLTHROUGH */
	case DB_EVENT_REP_NEWMASTER:
		case DB_EVENT_REP_PERM_FAILED:
		// I don't care about this one, for now.
		break;

	default:
		dbenv->errx("ignoring event %d", which);
	}
}

void RepQuoteExample::print_stocks(Db *dbp) {
	StringDbt key, data;
#define	MAXKEYSIZE	10
#define	MAXDATASIZE	20
	char keybuf[MAXKEYSIZE + 1], databuf[MAXDATASIZE + 1];
	char *kbuf, *dbuf;

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	kbuf = keybuf;
	dbuf = databuf;

	DbcAuto dbc(dbp, 0, 0);
	cout << "\tSymbol\tPrice" << endl
		<< "\t======\t=====" << endl;

	for (int ret = dbc->get(&key, &data, DB_FIRST);
		ret == 0;
		ret = dbc->get(&key, &data, DB_NEXT)) {
		key.get_string(&kbuf, MAXKEYSIZE);
		data.get_string(&dbuf, MAXDATASIZE);

		cout << "\t" << keybuf << "\t" << databuf << endl;
	}
	cout << endl << flush;
	dbc.close();
}

static void usage() {
	cerr << "usage: " << progname << endl
	    << "[-h home][-o host:port][-m host:port][-f host:port]"
		<< "[-n nsites][-p priority]" << endl;

	cerr << "\t -m host:port (required; m stands for me)" << endl
	    << "\t -o host:port (optional; o stands for other; any "
	    << "number of these may be specified)" << endl
	    << "\t -h home directory" << endl
	    << "\t -n nsites (optional; number of sites in replication "
	    << "group; defaults to 0" << endl
	    << "\t	in which case we try to dynamically compute the "
	    << "number of sites in" << endl
	    << "\t	the replication group)" << endl
	    << "\t -p priority (optional: defaults to 100)" << endl;

	exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
	RepConfigInfo config;
	char ch, *portstr, *tmphost;
	int tmpport;
	bool tmppeer;

	// Extract the command line parameters
	while ((ch = getopt(argc, argv, "Cf:h:Mm:n:o:p:v")) != EOF) {
		tmppeer = false;
		switch (ch) {
		case 'C':
			config.start_policy = DB_REP_CLIENT;
			break;
		case 'h':
			config.home = optarg;
			break;
		case 'M':
			config.start_policy = DB_REP_MASTER;
			break;
		case 'm':
			config.this_host.host = strtok(optarg, ":");
			if ((portstr = strtok(NULL, ":")) == NULL) {
				cerr << "Bad host specification." << endl;
				usage();
			}
			config.this_host.port = (unsigned short)atoi(portstr);
			config.got_listen_address = true;
			break;
		case 'n':
			config.totalsites = atoi(optarg);
			break;
		case 'f':
			tmppeer = true; // FALLTHROUGH
		case 'o':
			tmphost = strtok(optarg, ":");
			if ((portstr = strtok(NULL, ":")) == NULL) {
				cerr << "Bad host specification." << endl;
				usage();
			}
			tmpport = (unsigned short)atoi(portstr);

			config.addOtherHost(tmphost, tmpport, tmppeer);

			break;
		case 'p':
			config.priority = atoi(optarg);
			break;
		case 'v':
			config.verbose = true;
			break;
		case '?':
		default:
			usage();
		}
	}

	// Error check command line.
	if ((!config.got_listen_address) || config.home == NULL)
		usage();

	RepQuoteExample runner;
	try {
		runner.init(&config);
		runner.doloop();
	} catch (DbException dbe) {
		cerr << "Caught an exception during initialization or"
			<< " processing: " << dbe.what() << endl;
	}
	runner.terminate();
	return 0;
}
