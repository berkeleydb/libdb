/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.ReplicationHostAddress;
import com.sleepycat.db.ReplicationManagerStartPolicy;
import com.sleepycat.db.ReplicationManagerSiteInfo;
import com.sleepycat.db.VerboseConfig;

import java.io.File;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

public class TestRemoteSiteConfig {
    private static final String TEST_DIR_NAME = "TESTDIR";

    private File testdir;
    private EnvironmentConfig ec;
    private int[] testPorts;

    @Before public void setUp() throws Exception {
        testPorts = Util.findAvailablePorts(3);
        int localPort = testPorts[0];

        testdir = new File(TEST_DIR_NAME);
        Util.rm_rf(testdir);
        testdir.mkdir();

        ec = new EnvironmentConfig();
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
        ec.setReplicationManagerLocalSite(new ReplicationHostAddress("localhost", localPort));
    }

    @Test public void dynamicAdd() throws Exception {
        int remotePort1 = testPorts[1];
        ec.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", remotePort1), false);

        Environment env = new Environment(mkdir("."), ec);
        env.replicationManagerStart(1, ReplicationManagerStartPolicy.REP_ELECTION);

        Thread.sleep(2000);

        EnvironmentConfig ec2 = env.getConfig();
        int remotePort2 = testPorts[2];
        ec2.replicationManagerAddRemoteSite(new ReplicationHostAddress("localhost", remotePort2), false);
        env.setConfig(ec2);

        ReplicationManagerSiteInfo[] sites = env.getReplicationManagerSiteList();
        assertTrue(includes(sites, remotePort1));
        assertTrue(includes(sites, remotePort2));
        env.close();
    }

    private boolean includes(ReplicationManagerSiteInfo[] sites, int port) {
        for (int i=0; i<sites.length; i++)
            if (sites[i].addr.port == port)
                return true;
        return false;
    }

    public File mkdir(String dname) {
        File f = new File(testdir, dname);
        f.mkdir();
        return f;
    }
}



