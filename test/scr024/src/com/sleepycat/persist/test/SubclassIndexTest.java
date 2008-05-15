/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: SubclassIndexTest.java,v 1.1 2008/02/07 17:12:32 mark Exp $
 */

package com.sleepycat.persist.test;

import static com.sleepycat.persist.model.Relationship.MANY_TO_ONE;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Environment;
import com.sleepycat.db.EnvironmentConfig;
import com.sleepycat.persist.EntityCursor;
import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.PrimaryIndex;
import com.sleepycat.persist.SecondaryIndex;
import com.sleepycat.persist.StoreConfig;
import com.sleepycat.persist.model.Entity;
import com.sleepycat.persist.model.Persistent;
import com.sleepycat.persist.model.PrimaryKey;
import com.sleepycat.persist.model.SecondaryKey;
import com.sleepycat.util.test.SharedTestUtils;
import com.sleepycat.util.test.TestEnv;

public class SubclassIndexTest extends TestCase {

    private File envHome;
    private Environment env;

    public void setUp()
        throws IOException {

        envHome = new File(System.getProperty(SharedTestUtils.DEST_DIR));
        SharedTestUtils.emptyDir(envHome);
    }

    public void tearDown()
        throws IOException {

        if (env != null) {
            try {
                env.close();
            } catch (DatabaseException e) {
                System.out.println("During tearDown: " + e);
            }
        }
        try {
            SharedTestUtils.emptyDir(envHome);
        } catch (Error e) {
            System.out.println("During tearDown: " + e);
        }
        envHome = null;
        env = null;
    }

    public void testSubclassIndex()
        throws IOException, DatabaseException {

        EnvironmentConfig envConfig = TestEnv.BDB.getConfig();
        envConfig.setAllowCreate(true);
        env = new Environment(envHome, envConfig);

        StoreConfig storeConfig = new StoreConfig();
        storeConfig.setAllowCreate(true);
        EntityStore store = new EntityStore(env, "foo", storeConfig);

        PrimaryIndex<String, Employee> employeesById =
            store.getPrimaryIndex(String.class, Employee.class);

        employeesById.put(new Employee("1"));
        employeesById.put(new Manager("2", "a"));
        employeesById.put(new Manager("3", "a"));
        employeesById.put(new Manager("4", "b"));

        Employee e;
        Manager m;

        e = employeesById.get("1");
        assertNotNull(e);
        assertTrue(!(e instanceof Manager));

        /* Ensure DB exists BEFORE calling getSubclassIndex. [#15247] */
        PersistTestUtils.assertDbExists
            (true, env, "foo", Employee.class.getName(), "dept");

        /* Normal use: Subclass index for a key in the subclass. */
        SecondaryIndex<String, String, Manager> managersByDept =
            store.getSubclassIndex
                (employeesById, Manager.class, String.class, "dept");

        m = managersByDept.get("a");
        assertNotNull(m);
        assertEquals("2", m.id);

        m = managersByDept.get("b");
        assertNotNull(m);
        assertEquals("4", m.id);

        EntityCursor<Manager> managers = managersByDept.entities();
        try {
            m = managers.next();
            assertNotNull(m);
            assertEquals("2", m.id);
            m = managers.next();
            assertNotNull(m);
            assertEquals("3", m.id);
            m = managers.next();
            assertNotNull(m);
            assertEquals("4", m.id);
            m = managers.next();
            assertNull(m);
        } finally {
            managers.close();
        }

        /* Getting a subclass index for the entity class is also allowed. */
        store.getSubclassIndex
            (employeesById, Employee.class, String.class, "other");

        /* Getting a subclass index for a base class key is not allowed. */
        try {
            store.getSubclassIndex
                (employeesById, Manager.class, String.class, "other");
            fail();
        } catch (IllegalArgumentException expected) {
        }

        store.close();
        env.close();
        env = null;
    }

    @Entity
    private static class Employee {

        @PrimaryKey
        String id;

        @SecondaryKey(relate=MANY_TO_ONE)
        String other;

        Employee(String id) {
            this.id = id;
        }

        private Employee() {}
    }

    @Persistent
    private static class Manager extends Employee {

        @SecondaryKey(relate=MANY_TO_ONE)
        String dept;

        Manager(String id, String dept) {
            super(id);
            this.dept = dept;
        }

        private Manager() {}
    }
}
