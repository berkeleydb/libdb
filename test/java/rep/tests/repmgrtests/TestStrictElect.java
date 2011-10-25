/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Test;
import org.junit.Before;

import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.EventHandlerAdapter;
import com.sleepycat.db.ReplicationConfig;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.ReplicationStats;
import com.sleepycat.db.ReplicationTimeoutType;
import com.sleepycat.db.StatsConfig;
import com.sleepycat.db.VerboseConfig;

/**
 * Tests for repmgr's handling of elections, and the "strict 2-site"
 * config flag.
 */
public class TestStrictElect {
    private int[] testPorts;
    private int masterPort;
    private int clientPort;

    @Before public void setUp() throws Exception {
        testPorts = Util.findAvailablePorts(2);
        masterPort = testPorts[0];
        clientPort = testPorts[1];
    }
    
    /**
     * Verifies that by default in a 2-site group, client takes over
     * when master seems to have failed.
     */
    @Test public void liberal() throws Exception {
        EnvironmentConfig ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
        ec.setReplicationNumSites(2);
        File masterDir = Util.mkdir("master");
        Environment master = new Environment(masterDir, ec);
        master.setReplicationConfig(ReplicationConfig.STRICT_2SITE, true);
        master.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_MASTER);

        ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", clientPort));
        ec.setReplicationNumSites(2);
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", masterPort),
                                           false);
        MyEventHandler mon = new MyEventHandler();
        ec.setEventHandler(mon);
        File clientDir = Util.mkdir("client");
        Environment client = new Environment(clientDir, ec);
        client.setReplicationConfig(ReplicationConfig.STRICT_2SITE, true);
        client.setReplicationTimeout(ReplicationTimeoutType.ELECTION_RETRY, 500000);
        client.setReplicationTimeout(ReplicationTimeoutType.ELECTION_TIMEOUT, 500000);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);

        mon.await();

        // check stats, at this point there should be 0 elections
        ReplicationStats stats = client.getReplicationStats(StatsConfig.DEFAULT);
        assertTrue(stats.getElections() == 0);
        
        master.close();

        // wait a little while
        // there should be a couple of failed elections, and we should
        // not be master
        Thread.sleep(5000);
        stats = client.getReplicationStats(StatsConfig.DEFAULT);
        assertTrue(stats.getElections() > 2);
        assertTrue(stats.getEnvId() != stats.getMaster());

        client.close();
    }

    /**
     * Verifies that when the "strict" setting is turned on, failures
     * of a master leaves the group with no master: the client does
     * not take over.
     */
    @Test public void strict() throws Exception {
        EnvironmentConfig ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
        ec.setReplicationNumSites(2);
        File masterDir = Util.mkdir("master");
        Environment master = new Environment(masterDir, ec);
        master.setReplicationConfig(ReplicationConfig.STRICT_2SITE, true);
        master.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_MASTER);

        ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", clientPort));
        ec.setReplicationNumSites(2);
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", masterPort),
                                           false);
        MyEventHandler mon = new MyEventHandler();
        ec.setEventHandler(mon);
        File clientDir = Util.mkdir("client");
        Environment client = new Environment(clientDir, ec);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);

        mon.await();

        // check stats, at this point there should be 0 elections
        ReplicationStats stats = client.getReplicationStats(StatsConfig.DEFAULT);
        assertTrue(stats.getElections() == 0);
        
        master.close();

        Thread.sleep(3000);
        stats = client.getReplicationStats(StatsConfig.DEFAULT);
        assertTrue(stats.getElections() == 1);
        assertTrue(stats.getEnvId() == stats.getMaster());

        client.close();
    }

    /**
     * Verifies that the usual strict majority rule is observed in a
     * group with more than two sites, regardless of the config setting.
     */
    @Test public void threeSite() throws Exception {
        EnvironmentConfig ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", masterPort));
        ec.setReplicationNumSites(3);
        File masterDir = Util.mkdir("master");
        Environment master = new Environment(masterDir, ec);
        master.setReplicationConfig(ReplicationConfig.STRICT_2SITE, true);
        master.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_MASTER);

        ec = makeBasicConfig();
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", clientPort));
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", masterPort),
                                           false);
        MyEventHandler mon = new MyEventHandler();
        ec.setEventHandler(mon);
        ec.setReplicationNumSites(3);
        File clientDir = Util.mkdir("client");
        Environment client = new Environment(clientDir, ec);
        client.setReplicationTimeout(ReplicationTimeoutType.ELECTION_RETRY, 500000);
        client.setReplicationTimeout(ReplicationTimeoutType.ELECTION_TIMEOUT, 500000);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);

        mon.await();

        // check stats, at this point there should be 0 elections
        ReplicationStats stats = client.getReplicationStats(StatsConfig.DEFAULT);
        assertTrue(stats.getElections() == 0);
        
        master.close();

        // wait a little while
        // there should be a couple of failed elections, and we should
        // not be master
        Thread.sleep(5000);
        stats = client.getReplicationStats(StatsConfig.DEFAULT);
        assertTrue(stats.getElections() > 2);
        assertTrue(stats.getEnvId() != stats.getMaster());

        client.close();
    }

    private EnvironmentConfig makeBasicConfig() throws Exception {
        EnvironmentConfig ec = new EnvironmentConfig();
        ec.setAllowCreate(true);
        ec.setInitializeCache(true);
        ec.setInitializeLocking(true);
        ec.setInitializeLogging(true);
        ec.setInitializeReplication(true);
        ec.setTransactional(true);
        ec.setThreaded(true);
        if (Boolean.getBoolean("VERB_REPLICATION"))
            ec.setVerbose(VerboseConfig.REPLICATION, true);
        return (ec);
    }

    class MyEventHandler extends EventHandlerAdapter {
        private boolean done = false;
        private boolean panic = false;
        
        @Override synchronized public void handleRepStartupDoneEvent() {
                done = true;
                notifyAll();
            }
        
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
}
