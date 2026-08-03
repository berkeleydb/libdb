---
title: "Processing Loop"
api-name: "Processing Loop"
source: docs/gsg_db_rep/CXX/processingloop.html
---
## Processing Loop

Typically the central part of any replication application is some sort of a continuous loop that constantly checks the state of the environment (whether it is a replica or a master), opens and/or closes the databases as is necessary, and performs other useful work. A loop such as this one must of necessity take special care to know whether it is operating on a master or a replica environment because all of its activities are dependent upon that state.

The flow of activities through the loop will generally be as follows:

1.  Check whether the environment has changed state. If it has, you might want to reopen your database handles, especially if you opened your replica's database handles as read-only. In this case, you might need to reopen them as read-write. However, if you always open your database handles as read-write, then it is not automatically necessary to reopen the databases due to a state change. Instead, you could check for a `DB_REP_HANDLE_DEAD` return code when you use your database handle(s). If you see this, then you need to reopen your database handle(s).

2.  If the databases are closed, create new database handles, configure the handle as is appropriate, and then open the databases. Note that handle configuration will be different, depending on whether the handle is opened as a replica or a master. At a minimum, the master should be opened with database creation privileges, whereas the replica does not need to be. You must also open the master such that its databases are read-write. You <span class="emphasis">*can*</span> open replicas with read-only databases, so long as you are prepared to close and then reopen the handle in the event the client becomes a master.

    Also, note that if the local environment is a replica, then it is possible that databases do not currently exist. In this case, the database open attempts will fail. Your code will have to take this corner case into account (described below).

3.  Once the databases are opened, check to see if the local environment is a master. If it is, do whatever it is a master should do for your application.

    Remember that the code for your master should include some way for you to tell the master to exit gracefully.

4.  If the local environment is not a master, then do whatever it is your replica environments should do. Again, like the code for your master environments, you should provide a way for your replicas to exit the processing loop gracefully.

The following code fragment illustrates these points (note that we fill out this fragment with a working example next in this chapter):

``` c
/* loop to manage replication activities */

Db *dbp;
int ret;
APP_DATA *app_data;
u_int32_t flags;

dbp = NULL;
ret = 0;

/* 
 * Remember that for this to work, an APP_DATA struct would have first
 * had to been set to the environment handle's app_private data
 * member. (dbenv is presumably declared and opened in another part of
 * the code.)
 */
app_data = dbenv->get_app_private();

/* 
 * Infinite loop. We exit depending on how the master and replica code
 * is written.
 */
for (;;) {
    /* If dbp is not opened, we need to open it. */
    if (dbp == 0) {
        /* 
         * Create the handle and then configure it. Before you open
         * it, you have to decide what open flags to use:
         */
         dbp = new Db(&dbenv, 0);

        /*
         * Now you can open your database handle.
         *
         * One thing to watch out for is a case where the databases 
         * you are trying to open do not yet exist. This can happen 
         * for replicas where the databases are being opened 
         * read-only. If this happens, ENOENT is returned by the 
         * open() call.
         */
         try {
            dbp->open(NULL, DATABASE, NULL, DB_BTREE,
                    app_data->is_master ? DB_CREATE | DB_AUTO_COMMIT :
                    DB_AUTO_COMMIT, 0);
         } catch(DbException dbe) {
            if (dbe.get_errno() == ENOENT) {
                cout << "No stock db available yet - retrying." << endl;
                try {
                    dbp->close(0);
                } catch (DbException dbe2) {
                    cout << "Unexpected error closing after failed" <<
                            " open, message: " << dbe2.what() << endl;
                    dbp = NULL;
                    goto err;
                 }
                 dbp = NULL;
                 sleep(SLEEPTIME);
                 continue;
            } else {
                dbenv.err(ret, "DB->open");
                throw dbe;
            }
         }
    }

    /*
     * Now that the databases have been opened, continue with general
     * processing, depending on whether we are a master or a replica.
     */
     if (app_data->is_master) {
        /* 
         * Do master stuff here. Don't forget to include a way to
         * gracefully exit the loop. */
         */
     } else {
        /* 
         * Do replica stuff here. As is the case with the master
         * code, be sure to include a way to gracefully exit the
         * loop. 
         */
     }
} 
```
