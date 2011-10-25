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

import org.junit.After;
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
 * Test that a second client can be created and do internal init,
 * while the first thread is still clogged up.  Hmm, why did we think
 * this would be useful?
 */
public class TestDrainIntInit {
    private static final String TEST_DIR_NAME = "TESTDIR";

    private File testdir;
    private byte[] data;
    private Process fiddler;
    private int masterPort;
    private int clientPort;
    private int clientPort2;
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
            clientPort2 = 6002;
            masterSpoofPort = 7000;
            mgrPort = 8000;
            fiddler = null;
        } else {
            PortsConfig p = new PortsConfig(3);
            masterPort = p.getRealPort(0);
            masterSpoofPort = p.getSpoofPort(0);
            clientPort = p.getRealPort(1);
            clientPort2 = p.getRealPort(2);
            mgrPort = p.getManagerPort();
            fiddler = Util.startFiddler(p, getClass().getName());
        }
    }

    @After public void tearDown() throws Exception {
        if (fiddler != null) { fiddler.destroy(); }
    }
	
    // with only a single msg processing thread, clog up client,
    // verify that thread eventually gives up, by starting another
    // client and making sure it can do internal init.  Can we also
    // check that the amount of time it waits before giving up seems
    // reasonable?
    // 
    @Test public void drainBlockInternalInit() throws Exception {
        EnvironmentConfig ec = makeBasicConfig();
        ec.setReplicationLimit(100000000);
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
        MyEventHandler masterMonitor = new MyEventHandler();
        ec.setEventHandler(masterMonitor);
        Environment master = new Environment(mkdir("master"), ec);
        master.setReplicationTimeout(ReplicationTimeoutType.ACK_TIMEOUT,
                                     3000000);
        master.replicationManagerStart(2, ReplicationManagerStartPolicy.REP_MASTER);
		
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
        db.close();

        // create client, but don't sync yet
        // 
        ec = makeBasicConfig();
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


        ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", clientPort2));
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", masterSpoofPort), false);
        MyEventHandler mon = new MyEventHandler();
        ec.setEventHandler(mon);
        long start = System.currentTimeMillis();
        Environment client2 = new Environment(mkdir("client2"), ec);
        client2.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);

        mon.await();
        long duration = System.currentTimeMillis() - start;

        assertTrue("duration: " + duration, duration < 5000);

        // TODO: close client work-around
        client2.close();
        master.close();

        w.write("shutdown\r\n");
        w.flush();
        assertEquals("ok", br.readLine());
        s.close();
        fiddler = null;
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
