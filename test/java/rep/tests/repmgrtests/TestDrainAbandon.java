/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

import org.junit.Before;
import org.junit.Test;

import com.sleepycat.db.BtreeStats;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.EventHandlerAdapter;
import com.sleepycat.db.ReplicationConfig;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.ReplicationTimeoutType;
import com.sleepycat.db.VerboseConfig;

/**
 * Get a connection hopelessly clogged, and then kill the connection
 * (via fiddler closing socket, I guess).  Verify that the blocked
 * thread is immediately freed.
 */
public class TestDrainAbandon {
    private static final String TEST_DIR_NAME = "TESTDIR";

    private File testdir;
    private byte[] data;
    private int masterPort;
    private int clientPort;
    private int client2Port;
    private int masterSpoofPort;
    private int mgrPort;

    class MyEventHandler extends EventHandlerAdapter {
        private boolean done = false;
        private boolean panic = false;
        private int permFailCount = 0;
		
        @Override
            synchronized public void handleRepStartupDoneEvent() {
                done = true;
                notifyAll();
            }

        @Override
            synchronized public void handleRepPermFailedEvent() {
                permFailCount++;
            }

        synchronized public int getPermFailCount() { return permFailCount; }
		
        @Override
            synchronized public void handlePanicEvent() {
                done = true;
                panic = true;
                notifyAll();
            }

        synchronized void await() throws Exception {
            while (!done) { wait(); }
            if (panic)
                throw new Exception("aborted by panic in DB");
        }
    }

    @Before public void setUp() throws Exception {
        testdir = new File(TEST_DIR_NAME);
        Util.rm_rf(testdir);
        testdir.mkdir();

        String alphabet = "abcdefghijklmnopqrstuvwxyz";
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        OutputStreamWriter w = new OutputStreamWriter(baos);
        for (int i=0; i<100; i++) { w.write(alphabet); }
        w.close();
        data = baos.toByteArray();

        if (Boolean.getBoolean("MANUAL_FIDDLER_START")) {
            masterPort = 6000;
            clientPort = 6001;
            client2Port = 6002;
            masterSpoofPort = 7000;
            mgrPort = 8000;
        } else {
            PortsConfig p = new PortsConfig(3);
            masterPort = p.getRealPort(0);
            masterSpoofPort = p.getSpoofPort(0);
            clientPort = p.getRealPort(1);
            client2Port = p.getRealPort(2);
            mgrPort = p.getManagerPort();
            System.out.println("setUp: " + mgrPort + "/" + p.getFiddlerConfig());
            Util.startFiddler(p, getClass().getName());
        }
    }
	
    @Test public void testDraining() throws Exception {
        EnvironmentConfig masterConfig = makeBasicConfig();
        masterConfig.setReplicationLimit(100000000);
        masterConfig.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
        MyEventHandler masterMonitor = new MyEventHandler();
        masterConfig.setEventHandler(masterMonitor);
        Environment master = new Environment(mkdir("master"), masterConfig);
        master.setReplicationTimeout(ReplicationTimeoutType.ACK_TIMEOUT,
                                     30000000);
        master.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_MASTER);
		
        DatabaseConfig dc = new DatabaseConfig();
        dc.setTransactional(true);
        dc.setAllowCreate(true);
        dc.setType(DatabaseType.BTREE);
        dc.setPageSize(4096);
        Database db = master.openDatabase(null, "test.db", null, dc);
		
        DatabaseEntry key = new DatabaseEntry();
        DatabaseEntry value = new DatabaseEntry();
        value.setData(data);
        for (int i=0; i<120; i++) {
            String k = "The record number is: " + i;
            key.setData(k.getBytes());
            db.put(null, key, value);
        }

        BtreeStats stats = (BtreeStats)db.getStats(null, null);
        assertTrue(stats.getPageCount() >= 50);

        // create client, but don't sync yet
        // 
        EnvironmentConfig ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", clientPort));
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", masterSpoofPort), false);
        Environment client = new Environment(mkdir("client"), ec);
        client.setReplicationConfig(ReplicationConfig.DELAYCLIENT, true);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);
        Thread.sleep(2000);     // FIXME


        // tell fiddler to stop reading once it sees a PAGE message
        Socket s = new Socket("localhost", mgrPort);
        OutputStreamWriter w = new OutputStreamWriter(s.getOutputStream());

        String path1 = "{" + masterPort + "," + clientPort + "}"; // looks like {6000,6001}
        w.write("{" + path1 + ",page_clog}\r\n");
        w.flush();
        BufferedReader br = new BufferedReader(new InputStreamReader(s.getInputStream()));
        assertEquals("ok", br.readLine());

        client.syncReplication();

        // wait til it gets stuck
        Thread.sleep(5000);     // FIXME

        // With the connection stuck, the master cannot write out log
        // records for new "live" transactions.  Knowing we didn't
        // write the record, we should not bother waiting for an ack
        // that cannot possibly arrive; so we should simply return
        // quickly.  The duration should be very quick, but anything
        // less than the ack timeout indicates correct behavior (in
        // case this test runs on a slow, overloaded system).
        // 
        long startTime = System.currentTimeMillis();
        key.setData("one extra record".getBytes());
        db.put(null, key, value);
        long duration = System.currentTimeMillis() - startTime;
        assertTrue("txn duration: " + duration, duration < 29000);
        db.close();

        // Tell fiddler to close this connection.  That should trigger
        // us to abandon the timeout.  Then create another client and
        // see that it can complete its internal init quickly.  Since
        // we only have one thread at the master, this demonstrates
        // that the thread was abandoned.
        //
        String path2 = "{" + clientPort + "," + masterPort + "}"; // looks like {6001,6000}
        w.write("{" + path2 + ",shutdown}\r\n");
        w.flush();
        assertEquals("ok", br.readLine());

        ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", client2Port));
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", masterSpoofPort), false);
        MyEventHandler clientMonitor = new MyEventHandler();
        ec.setEventHandler(clientMonitor);
        Environment client2 = new Environment(mkdir("client2"), ec);
        startTime = System.currentTimeMillis();
        client2.replicationManagerStart(2, ReplicationManagerStartPolicy.REP_CLIENT);
        clientMonitor.await();
        duration = System.currentTimeMillis() - startTime;
        assertTrue("sync duration: " + duration, duration < 20000); // 20 seconds should be plenty

        client2.close();
        master.close();

        w.write("shutdown\r\n");
        w.flush();
        assertEquals("ok", br.readLine());
        s.close();
    }
	
    public static EnvironmentConfig makeBasicConfig() {
        EnvironmentConfig ec = new EnvironmentConfig();
        ec.setAllowCreate(true);
        ec.setInitializeCache(true);
        ec.setInitializeLocking(true);
        ec.setInitializeLogging(true);
        ec.setInitializeReplication(true);
        ec.setTransactional(true);
        ec.setThreaded(true);
        ec.setReplicationNumSites(3);
        if (Boolean.getBoolean("VERB_REPLICATION"))
            ec.setVerbose(VerboseConfig.REPLICATION, true);
        return (ec);
    }
	
    public File mkdir(String dname) {
        File f = new File(testdir, dname);
        f.mkdir();
        return f;
    }
}
