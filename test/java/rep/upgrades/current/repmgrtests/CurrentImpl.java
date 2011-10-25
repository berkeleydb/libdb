/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import com.sleepycat.db.CheckpointConfig;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.EventHandlerAdapter;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerAckPolicy;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.ReplicationTimeoutType;
import com.sleepycat.db.ReplicationManagerStats;
import com.sleepycat.db.ReplicationManagerSiteInfo;
import com.sleepycat.db.ReplicationStats;
import com.sleepycat.db.StatsConfig;
import com.sleepycat.db.VerboseConfig;

import static org.junit.Assert.*;
import java.io.File;

public class CurrentImpl implements TestMixedHeartbeats.Ops, TestReverseConnect.Ops {
    private Config config;
    private Environment[] envs = new Environment[2];
    private Environment client;

    class MyEventHandler extends EventHandlerAdapter {
        private boolean done = false;
        private boolean panic = false;
        private int permFailCount = 0;
        private int newmasterCount = 0;
		
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

        @Override synchronized public void handleRepNewMasterEvent(int eid) {
            newmasterCount++;
            notifyAll();
        }

        synchronized public int getNewmasterCount() {
            return newmasterCount;
        }

        synchronized void awaitNewmaster(Environment site, long deadline)
            throws Exception
        {
            long now;

            for (;;) {
                ReplicationStats s = site.getReplicationStats(StatsConfig.DEFAULT);

                // am I the master?
                // 
                if (s.getEnvId() == s.getMaster()) { break; }
                
                if ((now = System.currentTimeMillis()) >= deadline)
                    throw new Exception("aborted by timeout");
                long waitTime = deadline-now;
                wait(waitTime);
                if (panic)
                    throw new Exception("aborted by panic in DB");
            }
        }
		
        synchronized void await() throws Exception {
            while (!done) { wait(); }
            if (panic)
                throw new Exception("aborted by panic in DB");
        }
    }

    public void setConfig(Config c) { config = c; }

    public void joinExistingClient(int site, boolean configMaster, boolean useHB) throws Exception {
        EnvironmentConfig ec = makeBasicConfig(2);

        int p = config.getMyPort(site);
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", p));
        if (configMaster) {
            p = config.getOtherPort(site);
            ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", p), false);
        }
        MyEventHandler monitor = new MyEventHandler();
        ec.setEventHandler(monitor);
        File clientDir = new File(config.getBaseDir(), "dir" + site);
        assertTrue(clientDir.exists());
        client = new Environment(clientDir, ec);

        if (useHB) {
            client.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_SEND,
                                         3000000);
            client.setReplicationTimeout(ReplicationTimeoutType.HEARTBEAT_MONITOR,
                                         6000000);
        }

        envs[site] = client;
        client.setReplicationTimeout(ReplicationTimeoutType.CONNECTION_RETRY,
                                     1000000); // be impatient
        client.replicationManagerStart(3, ReplicationManagerStartPolicy.REP_CLIENT);
        monitor.await();

        assertTrue(client.getReplicationStats(StatsConfig.DEFAULT).getStartupComplete());
    }

    /**
     * Passively waits to be chosen as new master in an election.
     */
    public void becomeMaster(int siteId) throws Exception {
        Environment site = envs[siteId];
        MyEventHandler monitor = (MyEventHandler)site.getConfig().getEventHandler();
        long deadline = System.currentTimeMillis() + 5000;
        monitor.awaitNewmaster(site, deadline);
    }

    public void checkpoint(int site) throws Exception {
        assertNull(envs[site]);

        EnvironmentConfig ec = makeBasicConfig(2);
        File dir = new File(config.getBaseDir(), "dir" + site);
        assertTrue(dir.exists());
        Environment e = new Environment(dir, ec);
        CheckpointConfig cc = new CheckpointConfig();
        cc.setForce(true);
        e.checkpoint(cc);
        e.close();
    }

    public MyStats getStats(int site) throws Exception {
        MyStats s = new MyStats();
        ReplicationStats rs = envs[site].getReplicationStats(StatsConfig.DEFAULT);
        s.egen = rs.getElectionGen();
        s.elections = rs.getElections();
        s.envId = rs.getEnvId();
        s.master = rs.getMaster();
        ReplicationManagerStats rms = envs[site].getReplicationManagerStats(StatsConfig.DEFAULT);
        s.permFailedCount = rms.getPermFailed();
        return s;
    }
        
    public void checkMinVersion() throws Exception {
        // For this test to make sense, we must be at version 4.7 or higher
        // 
        int major = Environment.getVersionMajor();
        int minor = Environment.getVersionMinor();
        assertTrue(major > 4 || (major == 4 && minor >= 7));
    }

    public boolean writeUpdates(int siteId, int txnCount) throws Exception {
        throw new Exception("not yet implemented");
    }

    public void shutDown(int siteId) throws Exception {
        envs[siteId].close();
        envs[siteId] = null;
    }

    // Actually, this is now a bit weird, since we'd never even have
    // returned from the call to mon.await() above if we didn't get connected.
    // 
    public void verifyConnect(int site) throws Exception {
        for (int i=0; i<10; i++) {
            // check it
            ReplicationManagerSiteInfo[] si = envs[site].getReplicationManagerSiteList();
            if (si.length > 0) {
                System.out.println("connected to " + si[0].addr + " as EID " + si[0].eid);
                return;
            }
            Thread.sleep(1000);
        }
        fail("was not connected to anyone within 10 seconds");
    }

    private EnvironmentConfig makeBasicConfig(int nsites) {
        EnvironmentConfig ec = new EnvironmentConfig();
        ec.setAllowCreate(true);
        ec.setInitializeCache(true);
        ec.setInitializeLocking(true);
        ec.setInitializeLogging(true);
        ec.setInitializeReplication(true);
        ec.setTransactional(true);
        ec.setReplicationManagerAckPolicy(ReplicationManagerAckPolicy.ALL);
        ec.setRunRecovery(true);
        ec.setThreaded(true);
        ec.setReplicationNumSites(nsites);
//         ec.setErrorPrefix(... something? ...);
        if (Boolean.getBoolean("VERB_REPLICATION"))
            ec.setVerbose(VerboseConfig.REPLICATION, true);
        return (ec);
    }

}
