/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.EventHandlerAdapter;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.ReplicationManagerStats;
import com.sleepycat.db.ReplicationTimeoutType;
import com.sleepycat.db.StatsConfig;
import com.sleepycat.db.VerboseConfig;

/**
 * Tests for repmgr heartbeat feature.
 */
public class TestHeartbeats {
    private static final String TEST_DIR_NAME = "TESTDIR";

    private File testdir;
    private Process fiddler;
    Socket s;
    private int masterPort;
    private int clientPort;
    private int masterSpoofPort;
    private int client2Port;
    private int mgrPort;

    class MyEventHandler extends EventHandlerAdapter {
        private boolean done = false;
        private boolean panic = false;
        private int newmasterCount = 0;
        private boolean iAmMaster;
		
        @Override synchronized public void handleRepStartupDoneEvent() {
            done = true;
            notifyAll();
        }

        @Override synchronized public void handleRepMasterEvent() {
            iAmMaster = true;
        }

        @Override synchronized public void handleRepNewMasterEvent(int eid) {
            newmasterCount++;
        }

        synchronized public int getNewmasterCount() { return newmasterCount; }
        synchronized public boolean isMaster() { return iAmMaster; }

        @Override synchronized public void handlePanicEvent() {
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

        if (Boolean.getBoolean("MANUAL_FIDDLER_START")) {
            masterPort = 6000;
            clientPort = 6001;
            client2Port = 6002;
            masterSpoofPort = 7000;
            mgrPort = 8000;
            fiddler = null;
        } else {
            PortsConfig p = new PortsConfig(3);
            masterPort = p.getRealPort(0);
            masterSpoofPort = p.getSpoofPort(0);
            clientPort = p.getRealPort(1);
            client2Port = p.getRealPort(2);
            mgrPort = p.getManagerPort();
            fiddler = Util.startFiddler(p, getClass().getName());
        }
    }

    @After public void tearDown() throws Exception {
        if (fiddler != null) { fiddler.destroy(); }
    }

    // (Is 2 minutes enough?)
    // 
    @Test(timeout=120000) public void testMonitorCallElect() throws Exception {
        EnvironmentConfig masterConfig = makeBasicConfig();
        masterConfig.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
        MyEventHandler masterMonitor = new MyEventHandler();
        masterConfig.setEventHandler(masterMonitor);
        File masterDir = mkdir("master");
        Environment master = new Environment(masterDir, masterConfig);
        master.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_SEND, 3000000);
        master.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_MONITOR, 5000000);
        master.replicationManagerStart(3, ReplicationManagerStartPolicy.REP_MASTER);
        DatabaseConfig dc = new DatabaseConfig();
        dc.setTransactional(true);
        dc.setAllowCreate(true);
        dc.setType(DatabaseType.BTREE);
        Database db = master.openDatabase(null, "test.db", null, dc);
		

        // create two clients, wait for them to finish sync-ing up
        // 
        MyEventHandler clientMonitor = new MyEventHandler();
        EnvironmentConfig ec = makeClientConfig(clientMonitor, clientPort, masterSpoofPort);
        
        File clientDir = mkdir("client");
        Environment client = new Environment(clientDir, ec);
        client.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_SEND, 3000000);
        client.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_MONITOR, 5000000);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);
        clientMonitor.await();

        MyEventHandler client2Monitor = new MyEventHandler();
        ec = makeClientConfig(client2Monitor, client2Port, masterSpoofPort);
        
        File client2Dir = mkdir("client2");
        Environment client2 = new Environment(client2Dir, ec);
        client2.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_SEND, 3000000);
        client2.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_MONITOR, 5000000);
        client2.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);
        client2Monitor.await();

        // do some transactions, at a leisurely pace (but quicker than
        // the hb period)
        // 
        DatabaseEntry key = new DatabaseEntry();
        DatabaseEntry value = new DatabaseEntry();
        value.setData("foo".getBytes());
        for (int i=0; i<3; i++) {
            Thread.sleep(2000);
            String k = "The record number is: " + i;
            key.setData(k.getBytes());
            db.put(null, key, value);
        }

        // a few more, but not quick enough to prevent HB's
        //
        for (int i=3; i<6; i++) {
            Thread.sleep(4000);
            String k = "The record number is: " + i;
            key.setData(k.getBytes());
            db.put(null, key, value);
        }

        // pause for a few HB times
        Thread.sleep(10000);
        assertFalse(clientMonitor.isMaster() || client2Monitor.isMaster());

        // tell fiddler to freeze (not really the right word) the
        // connection paths from master to both clients.
        // 
        if (s == null)
            s = new Socket("localhost", mgrPort);
        OutputStreamWriter w = new OutputStreamWriter(s.getOutputStream());

        String path1 = "{" + masterPort + "," + clientPort + "}"; // looks like {6000,6001}
        w.write("{" + path1 + ",toss_all}\r\n");
        w.flush();
        BufferedReader br = new BufferedReader(new InputStreamReader(s.getInputStream()));
        assertEquals("ok", br.readLine());

        String path2 = "{" + masterPort + "," + client2Port + "}"; // looks like {6000,6002}
        w.write("{" + path2 + ",toss_all}\r\n");
        w.flush();
        assertEquals("ok", br.readLine());

        // wait for the clients to notice a problem (5 sec max), plus
        // give them a couple extra seconds to hold an election
        // 
        Thread.sleep(5000 + 2000);
        ReplicationManagerStats masterStats =
            master.getReplicationManagerStats(StatsConfig.DEFAULT);
        assertEquals(2, masterStats.getConnectionDrop());
        assertTrue(clientMonitor.isMaster() || client2Monitor.isMaster());

        client2.close();
        client.close();
        db.close();
        master.close();

        w.write("shutdown\r\n");
        w.flush();
        assertEquals("ok", br.readLine());
        s.close();
        fiddler = null;
    }

    // TODO: what happens when we "freeze" connection to only one of
    // the clients?  The loser would call for an election, but when
    // the other client joins, it would get smacked down by the
    // existing master.  I guess that means the loser client would
    // remain in the election loop, endlessly retrying.  I'm sure
    // that's the correct behavior, but I wonder if there's anything
    // worth testing about it.  Eventually it retries a connection to
    // the master, and I guess that succeeds.  I guess we could check
    // that that works, and that everything eventually settles down
    // again, including having the client eventually get all missing
    // data.
    // 
    // election timeout: 1 second
    // election retry: 1 second (?)
    // connection retry: 3-4 seconds?
    // 
    @Test public void disagreement() throws Exception {
    }

    private EnvironmentConfig makeClientConfig(MyEventHandler evHandler,
                                               int clientPort, int masterPort)
        throws Exception
    {
        EnvironmentConfig ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", clientPort));
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", masterPort), false);
        ec.setEventHandler(evHandler);
        return (ec);
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
