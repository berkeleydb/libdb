/*-
 * See the file LICENSE for redistribution information.
 * 
 * Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 */

package repmgrtests;

import static org.junit.Assert.assertTrue;

import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.db.ReplicationHostAddress;

import org.junit.Before;
import org.junit.Test;

import java.io.File;

public class TestConfig {
    private EnvironmentConfig ec;
    private File dir;

    @Before public void setUp() throws Exception {
        ec = new EnvironmentConfig();
        ec.setAllowCreate(true);
        ec.setInitializeCache(true);
        ec.setInitializeLocking(true);
        ec.setInitializeLogging(true);
        ec.setInitializeReplication(true);
        ec.setTransactional(true);
        ec.setThreaded(true);

        dir = Util.mkdir("site1");
    }

    @Test(expected=IllegalArgumentException.class)
        public void nullHost() throws Exception
    {
        ReplicationHostAddress addr = new ReplicationHostAddress(null, 6000);
        ec.setReplicationManagerLocalSite(addr);

        Environment env = new Environment(dir, ec);
        env.close();
    }

    @Test public void host() throws Exception
    {
        ReplicationHostAddress addr = new ReplicationHostAddress("localhost", 6000);
        ec.setReplicationManagerLocalSite(addr);

        Environment env = new Environment(dir, ec);
        env.close();
    }

    @Test public void gethost() throws Exception
    {
        ReplicationHostAddress addr = new ReplicationHostAddress("localhost", 6000);
        ec.setReplicationManagerLocalSite(addr);

        Environment env = new Environment(dir, ec);
	EnvironmentConfig cfg = env.getConfig();
	assertTrue(addr.host.equals(cfg.getReplicationManagerLocalSite().host));
	assertTrue(addr.port == cfg.getReplicationManagerLocalSite().port);
        env.close();
    }

}
