/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import com.sleepycat.db.CheckpointConfig;
import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.DatabaseType;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.EventHandlerAdapter;
import com.sleepycat.db.ReplicationConfig;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerAckPolicy;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.ReplicationTimeoutType;
import com.sleepycat.db.VerboseConfig;

import static org.junit.Assert.*;

public class ConnectScript implements SimpleConnectTest.Ops {
    private SimpleConnectTest.Config conf;
    private Environment client;

    public void setConfig(SimpleConnectTest.Config c) {
        conf = c;
    }

    public void upgradeClient() throws Exception {
        int[] remotePorts;
        if (conf.reverse)
            remotePorts = SimpleConnectTest.noRemotePorts;
        else {
            remotePorts = new int[1];
            remotePorts[0] = conf.masterPort;
        }
        EnvironmentConfig ec = makeBasicConfig(conf.clientPort, remotePorts);
        client = new Environment(conf.clientDir, ec);

        CheckpointConfig cc = new CheckpointConfig();
        cc.setForce(true);
        client.checkpoint(cc);
        client.close();
        
        MyEventHandler mon = new MyEventHandler();
        ec.setEventHandler(mon);
        client = new Environment(conf.clientDir, ec);

        configEachEnv(client);
        client.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_CLIENT);
        mon.awaitStartupDone();
    }

    public void shutdownClient() throws Exception {
        client.close();
    }

    private EnvironmentConfig makeBasicConfig(int myPort, int[] remotePorts)
        throws Exception
    {
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
        ec.setReplicationNumSites(2);

        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", myPort));
        for (int p : remotePorts) {
            ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", p),
                                               false);
        }
        
        if (Boolean.getBoolean("VERB_REPLICATION"))
            ec.setVerbose(VerboseConfig.REPLICATION, true);
        return (ec);
    }

    private void configEachEnv(Environment e) throws Exception {
        e.setReplicationTimeout(ReplicationTimeoutType.CONNECTION_RETRY,
                                1000000); // be impatient
        e.setReplicationConfig(ReplicationConfig.STRICT_2SITE, true);
    }

    class MyEventHandler extends EventHandlerAdapter {
        private boolean done = false;
        private boolean panic = false;

        @Override synchronized public void handleRepStartupDoneEvent() {
            done = true;
            notifyAll();
        }

        @Override synchronized public void handlePanicEvent() {
            panic = true;
            done = true;
            notifyAll();
        }

        synchronized void awaitStartupDone() throws Exception {
            long deadline = System.currentTimeMillis() + 10000;
            while (!done) {
                long now = System.currentTimeMillis();
                if (now >= deadline)
                    throw new Exception("timeout expired");
                long duration = deadline - now;
                wait(duration);
            }
            if (panic)
                throw new Exception("aborted by panic in DB");
        }
    }
}
