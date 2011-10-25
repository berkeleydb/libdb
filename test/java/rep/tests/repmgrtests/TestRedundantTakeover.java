/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import static org.junit.Assert.assertEquals;

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
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.EventHandlerAdapter;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerAckPolicy;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.VerboseConfig;

/**
 * A simple test of repmgr allowing a new, seemingly "redundant"
 * incoming connection to take over, giving up (closing) an existing
 * connection.
 */
public class TestRedundantTakeover {
    private static final String TEST_DIR_NAME = "TESTDIR";

    private File testdir;
    private Process fiddler;
    private int mgrPort;
    private int masterPort;
    private int clientPort;
    private int masterSpoofPort;

    class MyEventHandler extends EventHandlerAdapter {
        private boolean done = false;
        private boolean panic = false;
        private int permFailCount = 0;
		
        @Override synchronized public void handleRepStartupDoneEvent() {
            done = true;
            notifyAll();
        }

        @Override synchronized public void handleRepPermFailedEvent() {
            permFailCount++;
        }

        synchronized public int getPermFailCount() { return permFailCount; }

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
            masterSpoofPort = 7000;
            mgrPort = 8000;
            fiddler = null;
        } else {
            PortsConfig p = new PortsConfig(3);
            masterPort = p.getRealPort(0);
            masterSpoofPort = p.getSpoofPort(0);
            clientPort = p.getRealPort(1);
            mgrPort = p.getManagerPort();
            fiddler = Util.startFiddler(p, getClass().getName());
        }
    }

    @After public void tearDown() throws Exception {
        if (fiddler != null) { fiddler.destroy(); }
    }

    // 2nd try:
    // 
    @Test public void testExchange2() throws Exception {
        EnvironmentConfig masterConfig = makeBasicConfig();
        masterConfig.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
        MyEventHandler masterMonitor = new MyEventHandler();
        masterConfig.setEventHandler(masterMonitor);
        File masterDir = mkdir("master");
        Environment master = new Environment(masterDir, masterConfig);
        master.replicationManagerStart(3, ReplicationManagerStartPolicy.REP_MASTER);
		

        // create client, wait for it to finish sync-ing up
        // 
        MyEventHandler clientMonitor = new MyEventHandler();
        EnvironmentConfig ec = makeClientConfig(clientMonitor, clientPort, masterSpoofPort);
        
        File clientDir = mkdir("client");
        Environment client = new Environment(clientDir, ec);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);
        clientMonitor.await();

        // tell fiddler to freeze the connection without killing it
        // 
        Socket s = new Socket("localhost", mgrPort);
        OutputStreamWriter w = new OutputStreamWriter(s.getOutputStream());

        String path1 = "{" + clientPort + "," + masterPort + "}"; // looks like {6001,6000}
        w.write("{" + path1 + ",toss_all}\r\n");
        w.flush();
        BufferedReader br = new BufferedReader(new InputStreamReader(s.getInputStream()));
        assertEquals("ok", br.readLine());

        // close client (which will be hidden from master), and try
        // opening it again
        client.close();
        clientMonitor = new MyEventHandler();
        ec = makeClientConfig(clientMonitor, clientPort, masterSpoofPort);
        ec.setRunRecovery(true);
        client = new Environment(clientDir, ec);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_ELECTION);
        clientMonitor.await();

        // verify with fiddler that the old connection was closed.
        // TODO

        // do a new live transaction at the master, make sure
        // that can work.
        // 
        int initialCount = masterMonitor.getPermFailCount();
        DatabaseConfig dc2 = new DatabaseConfig();
        dc2.setTransactional(true);
        dc2.setAllowCreate(true);
        dc2.setType(DatabaseType.BTREE);
        Database db2 = master.openDatabase(null, "test2.db", null, dc2);
        db2.close();
        assertEquals(initialCount, masterMonitor.getPermFailCount());


        master.close();
        client.close();

        w.write("shutdown\r\n");
        w.flush();
        assertEquals("ok", br.readLine());
        s.close();
        fiddler = null;
    }

    // first try:
    // 
//     @Test public void testExchange() throws Exception {
//         int masterPort = 6000;
//         int clientPort = 6001;
//         int masterSpoofPort = 7000;
//         int clientSpoofPort = 7001;
		
//         EnvironmentConfig masterConfig = makeBasicConfig();
//         masterConfig.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
//         MyEventHandler masterMonitor = new MyEventHandler();
//         masterConfig.setEventHandler(masterMonitor);
//         File masterDir = mkdir("master");
//         Environment master = new Environment(masterDir, masterConfig);
//         master.replicationManagerStart(3, ReplicationManagerStartPolicy.REP_MASTER);
		

//         // create client, wait for it to finish sync-ing up
//         // 
//         MyEventHandler clientMonitor = new MyEventHandler();
//         EnvironmentConfig ec = makeClientConfig(clientMonitor, clientPort, masterSpoofPort);
        
//         File clientDir = mkdir("client");
//         Environment client = new Environment(clientDir, ec);
//         client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);
//         clientMonitor.await();

//         // tell fiddler to freeze the connection without killing it
//         // 
//         Socket s = new Socket("localhost", 8000);
//         OutputStreamWriter w = new OutputStreamWriter(s.getOutputStream());
//         w.write("{{6001,6000},toss_all}\r\n");
//         w.flush();
//         BufferedReader br = new BufferedReader(new InputStreamReader(s.getInputStream()));
//         assertEquals("ok", br.readLine());

//         // generate some traffic, just so's the damn fiddler gets a
//         // chance to wake up and see that we've sent it a command.
//         // (This needs some serious improvement.)  :-((
//         // 
//         DatabaseConfig dc = new DatabaseConfig();
//         dc.setTransactional(true);
//         dc.setAllowCreate(true);
//         dc.setType(DatabaseType.BTREE);
//         Database db = master.openDatabase(null, "test.db", null, dc);
//         db.close();

//         // close client (which will be hidden from master), and try
//         // opening it again
//         client.close();
//         clientMonitor = new MyEventHandler();
//         ec = makeClientConfig(clientMonitor, clientPort, masterSpoofPort);
//         client = new Environment(clientDir, ec);
//         client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);
//         clientMonitor.await();

//         // verify with fiddler that the old connection was closed.
//         // TODO

//         // do a new live transaction at the master, make sure
//         // that can work.
//         // 
//         int initialCount = masterMonitor.getPermFailCount();
//         DatabaseConfig dc2 = new DatabaseConfig();
//         dc2.setTransactional(true);
//         dc2.setAllowCreate(true);
//         dc2.setType(DatabaseType.BTREE);
//         Database db2 = master.openDatabase(null, "test2.db", null, dc);
//         db2.close();
//         assertEquals(initialCount+1, masterMonitor.getPermFailCount());


//         master.close();
//         client.close();

//         w.write("shutdown\r\n");
//         w.flush();
//         assertEquals("ok", br.readLine());
//         s.close();
//     }

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
        ec.setReplicationManagerAckPolicy(ReplicationManagerAckPolicy.ALL);
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
