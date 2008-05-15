/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: OperationTest.java,v 1.2 2008/02/12 19:15:26 mark Exp $
 */

package com.sleepycat.persist.test;

import static com.sleepycat.persist.model.Relationship.MANY_TO_ONE;
import static com.sleepycat.persist.model.Relationship.ONE_TO_MANY;
import static com.sleepycat.persist.model.Relationship.ONE_TO_ONE;
import static com.sleepycat.persist.model.DeleteAction.CASCADE;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.Test;

import com.sleepycat.db.Database;
import com.sleepycat.db.DatabaseConfig;
import com.sleepycat.db.DatabaseException;
import com.sleepycat.db.Transaction;
import com.sleepycat.persist.EntityCursor;
import com.sleepycat.persist.EntityIndex;
import com.sleepycat.persist.EntityStore;
import com.sleepycat.persist.PrimaryIndex;
import com.sleepycat.persist.SecondaryIndex;
import com.sleepycat.persist.StoreConfig;
import com.sleepycat.persist.impl.Store;
import com.sleepycat.persist.model.Entity;
import com.sleepycat.persist.model.KeyField;
import com.sleepycat.persist.model.Persistent;
import com.sleepycat.persist.model.PrimaryKey;
import com.sleepycat.persist.model.SecondaryKey;
import com.sleepycat.persist.raw.RawStore;
import com.sleepycat.util.test.TxnTestCase;

/**
 * Tests misc store and index operations that are not tested by IndexTest.
 *
 * @author Mark Hayes
 */
public class OperationTest extends TxnTestCase {

    private static final String STORE_NAME = "test";
 
    public static Test suite() {
        return txnTestSuite(OperationTest.class, null, null);
    }

    private EntityStore store;

    private void openReadOnly()
        throws DatabaseException {

        StoreConfig config = new StoreConfig();
        config.setReadOnly(true);
        open(config);
    }

    private void open()
        throws DatabaseException {

        StoreConfig config = new StoreConfig();
        config.setAllowCreate(envConfig.getAllowCreate());
        open(config);
    }

    private void open(StoreConfig config)
        throws DatabaseException {

        config.setTransactional(envConfig.getTransactional());
        store = new EntityStore(env, STORE_NAME, config);
    }

    private void close()
        throws DatabaseException {

        store.close();
        store = null;
    }

    /**
     * The store must be closed before closing the environment.
     */
    public void tearDown()
        throws Exception {

        try {
            if (store != null) {
                store.close();
            }
        } catch (Throwable e) {
            System.out.println("During tearDown: " + e);
        }
        store = null;
        super.tearDown();
    }

    public void testReadOnly()
        throws DatabaseException {

        open();
        PrimaryIndex<Integer,SharedSequenceEntity1> priIndex =
            store.getPrimaryIndex(Integer.class, SharedSequenceEntity1.class);
        Transaction txn = txnBegin();
        SharedSequenceEntity1 e = new SharedSequenceEntity1();
        priIndex.put(txn, e);
        assertEquals(1, e.key);
        txnCommit(txn);
        close();

        /*
         * Check that we can open the store read-only and read the records
         * written above.
         */
        openReadOnly();
        priIndex =
            store.getPrimaryIndex(Integer.class, SharedSequenceEntity1.class);
        e = priIndex.get(1);
        assertNotNull(e);
        close();
    }


    public void testUninitializedCursor()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,MyEntity> priIndex =
            store.getPrimaryIndex(Integer.class, MyEntity.class);

        Transaction txn = txnBeginCursor();

        MyEntity e = new MyEntity();
        e.priKey = 1;
        e.secKey = 1;
        priIndex.put(txn, e);

        EntityCursor<MyEntity> entities = priIndex.entities(txn, null);
        try {
            entities.nextDup();
            fail();
        } catch (IllegalStateException expected) {}
        try {
            entities.prevDup();
            fail();
        } catch (IllegalStateException expected) {}
        try {
            entities.current();
            fail();
        } catch (IllegalStateException expected) {}
        try {
            entities.delete();
            fail();
        } catch (IllegalStateException expected) {}
        try {
            entities.update(e);
            fail();
        } catch (IllegalStateException expected) {}
        try {
            entities.count();
            fail();
        } catch (IllegalStateException expected) {}

        entities.close();
        txnCommit(txn);
        close();
    }

    public void testCursorCount()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,MyEntity> priIndex =
            store.getPrimaryIndex(Integer.class, MyEntity.class);

        SecondaryIndex<Integer,Integer,MyEntity> secIndex =
            store.getSecondaryIndex(priIndex, Integer.class, "secKey");

        Transaction txn = txnBeginCursor();

        MyEntity e = new MyEntity();
        e.priKey = 1;
        e.secKey = 1;
        priIndex.put(txn, e);

        EntityCursor<MyEntity> cursor = secIndex.entities(txn, null);
        cursor.next();
        assertEquals(1, cursor.count());
        cursor.close();

        e.priKey = 2;
        priIndex.put(txn, e);
        cursor = secIndex.entities(txn, null);
        cursor.next();
        assertEquals(2, cursor.count());
        cursor.close();

        txnCommit(txn);
        close();
    }

    public void testCursorUpdate()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,MyEntity> priIndex =
            store.getPrimaryIndex(Integer.class, MyEntity.class);

        SecondaryIndex<Integer,Integer,MyEntity> secIndex =
            store.getSecondaryIndex(priIndex, Integer.class, "secKey");

        Transaction txn = txnBeginCursor();

        Integer k;
        MyEntity e = new MyEntity();
        e.priKey = 1;
        e.secKey = 2;
        priIndex.put(txn, e);

        /* update() with primary entity cursor. */
        EntityCursor<MyEntity> entities = priIndex.entities(txn, null);
        e = entities.next();
        assertNotNull(e);
        assertEquals(1, e.priKey);
        assertEquals(Integer.valueOf(2), e.secKey);
        e.secKey = null;
        assertTrue(entities.update(e));
        e = entities.current();
        assertNotNull(e);
        assertEquals(1, e.priKey);
        assertEquals(null, e.secKey);
        e.secKey = 3;
        assertTrue(entities.update(e));
        e = entities.current();
        assertNotNull(e);
        assertEquals(1, e.priKey);
        assertEquals(Integer.valueOf(3), e.secKey);
        entities.close();

        /* update() with primary keys cursor. */
        EntityCursor<Integer> keys = priIndex.keys(txn, null);
        k = keys.next();
        assertNotNull(k);
        assertEquals(Integer.valueOf(1), k);
        try {
            keys.update(2);
            fail();
        } catch (UnsupportedOperationException expected) {
        }
        keys.close();

        /* update() with secondary entity cursor. */
        entities = secIndex.entities(txn, null);
        e = entities.next();
        assertNotNull(e);
        assertEquals(1, e.priKey);
        assertEquals(Integer.valueOf(3), e.secKey);
        try {
            entities.update(e);
            fail();
        } catch (UnsupportedOperationException expected) {
        } catch (IllegalArgumentException expectedForDbCore) {
        }
        entities.close();

        /* update() with secondary keys cursor. */
        keys = secIndex.keys(txn, null);
        k = keys.next();
        assertNotNull(k);
        assertEquals(Integer.valueOf(3), k);
        try {
            keys.update(k);
            fail();
        } catch (UnsupportedOperationException expected) {
        }
        keys.close();

        txnCommit(txn);
        close();
    }

    public void testCursorDelete()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,MyEntity> priIndex =
            store.getPrimaryIndex(Integer.class, MyEntity.class);

        SecondaryIndex<Integer,Integer,MyEntity> secIndex =
            store.getSecondaryIndex(priIndex, Integer.class, "secKey");

        Transaction txn = txnBeginCursor();

        /* delete() with primary and secondary entities cursor. */

        for (EntityIndex index : new EntityIndex[] { priIndex, secIndex }) {

            MyEntity e = new MyEntity();
            e.priKey = 1;
            e.secKey = 1;
            priIndex.put(txn, e);
            e.priKey = 2;
            priIndex.put(txn, e);

            EntityCursor<MyEntity> cursor = index.entities(txn, null);

            e = cursor.next();
            assertNotNull(e);
            assertEquals(1, e.priKey);
            e = cursor.current();
            assertNotNull(e);
            assertEquals(1, e.priKey);
            assertTrue(cursor.delete());
            assertTrue(!cursor.delete());
            assertNull(cursor.current());

            e = cursor.next();
            assertNotNull(e);
            assertEquals(2, e.priKey);
            e = cursor.current();
            assertNotNull(e);
            assertEquals(2, e.priKey);
            assertTrue(cursor.delete());
            assertTrue(!cursor.delete());
            assertNull(cursor.current());

            e = cursor.next();
            assertNull(e);

            if (index == priIndex) {
                e = new MyEntity();
                e.priKey = 2;
                e.secKey = 1;
                assertTrue(!cursor.update(e));
            }

            cursor.close();
        }

        /* delete() with primary and secondary keys cursor. */

        for (EntityIndex index : new EntityIndex[] { priIndex, secIndex }) {

            MyEntity e = new MyEntity();
            e.priKey = 1;
            e.secKey = 1;
            priIndex.put(txn, e);
            e.priKey = 2;
            priIndex.put(txn, e);

            EntityCursor<Integer> cursor = index.keys(txn, null);

            Integer k = cursor.next();
            assertNotNull(k);
            assertEquals(1, k.intValue());
            k = cursor.current();
            assertNotNull(k);
            assertEquals(1, k.intValue());
            assertTrue(cursor.delete());
            assertTrue(!cursor.delete());
            assertNull(cursor.current());

            int expectKey = (index == priIndex) ? 2 : 1;
            k = cursor.next();
            assertNotNull(k);
            assertEquals(expectKey, k.intValue());
            k = cursor.current();
            assertNotNull(k);
            assertEquals(expectKey, k.intValue());
            assertTrue(cursor.delete());
            assertTrue(!cursor.delete());
            assertNull(cursor.current());

            k = cursor.next();
            assertNull(k);

            cursor.close();
        }

        txnCommit(txn);
        close();
    }

    public void testDeleteFromSubIndex()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,MyEntity> priIndex =
            store.getPrimaryIndex(Integer.class, MyEntity.class);

        SecondaryIndex<Integer,Integer,MyEntity> secIndex =
            store.getSecondaryIndex(priIndex, Integer.class, "secKey");

        Transaction txn = txnBegin();
        MyEntity e = new MyEntity();
        e.secKey = 1;
        e.priKey = 1;
        priIndex.put(txn, e);
        e.priKey = 2;
        priIndex.put(txn, e);
        e.priKey = 3;
        priIndex.put(txn, e);
        e.priKey = 4;
        priIndex.put(txn, e);
        txnCommit(txn);

        EntityIndex<Integer,MyEntity> subIndex = secIndex.subIndex(1);
        txn = txnBeginCursor();
        e = subIndex.get(txn, 1, null);
        assertEquals(1, e.priKey);
        assertEquals(Integer.valueOf(1), e.secKey);
        e = subIndex.get(txn, 2, null);
        assertEquals(2, e.priKey);
        assertEquals(Integer.valueOf(1), e.secKey);
        e = subIndex.get(txn, 3, null);
        assertEquals(3, e.priKey);
        assertEquals(Integer.valueOf(1), e.secKey);
        e = subIndex.get(txn, 5, null);
        assertNull(e);

        boolean deleted = subIndex.delete(txn, 1);
        assertTrue(deleted);
        assertNull(subIndex.get(txn, 1, null));
        assertNotNull(subIndex.get(txn, 2, null));

        EntityCursor<MyEntity> cursor = subIndex.entities(txn, null);
        boolean saw4 = false;
        for (MyEntity e2 = cursor.first(); e2 != null; e2 = cursor.next()) {
            if (e2.priKey == 3) {
                cursor.delete();
            }
            if (e2.priKey == 4) {
                saw4 = true;
            }
        }
        cursor.close();
        assertTrue(saw4);
        assertNull(subIndex.get(txn, 1, null));
        assertNull(subIndex.get(txn, 3, null));
        assertNotNull(subIndex.get(txn, 2, null));
        assertNotNull(subIndex.get(txn, 4, null));

        txnCommit(txn);
        close();
    }

    @Entity
    static class MyEntity {

        @PrimaryKey
        private int priKey;

        @SecondaryKey(relate=MANY_TO_ONE)
        private Integer secKey;

        private MyEntity() {}
    }

    public void testSharedSequence()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,SharedSequenceEntity1> priIndex1 =
            store.getPrimaryIndex(Integer.class, SharedSequenceEntity1.class);

        PrimaryIndex<Integer,SharedSequenceEntity2> priIndex2 =
            store.getPrimaryIndex(Integer.class, SharedSequenceEntity2.class);

        Transaction txn = txnBegin();
        SharedSequenceEntity1 e1 = new SharedSequenceEntity1();
        SharedSequenceEntity2 e2 = new SharedSequenceEntity2();
        priIndex1.put(txn, e1);
        assertEquals(1, e1.key);
        priIndex2.putNoOverwrite(txn, e2);
        assertEquals(Integer.valueOf(2), e2.key);
        e1.key = 0;
        priIndex1.putNoOverwrite(txn, e1);
        assertEquals(3, e1.key);
        e2.key = null;
        priIndex2.put(txn, e2);
        assertEquals(Integer.valueOf(4), e2.key);
        txnCommit(txn);

        close();
    }

    @Entity
    static class SharedSequenceEntity1 {

        @PrimaryKey(sequence="shared")
        private int key;
    }

    @Entity
    static class SharedSequenceEntity2 {

        @PrimaryKey(sequence="shared")
        private Integer key;
    }

    public void testSeparateSequence()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,SeparateSequenceEntity1> priIndex1 =
            store.getPrimaryIndex
                (Integer.class, SeparateSequenceEntity1.class);

        PrimaryIndex<Integer,SeparateSequenceEntity2> priIndex2 =
            store.getPrimaryIndex
                (Integer.class, SeparateSequenceEntity2.class);

        Transaction txn = txnBegin();
        SeparateSequenceEntity1 e1 = new SeparateSequenceEntity1();
        SeparateSequenceEntity2 e2 = new SeparateSequenceEntity2();
        priIndex1.put(txn, e1);
        assertEquals(1, e1.key);
        priIndex2.putNoOverwrite(txn, e2);
        assertEquals(Integer.valueOf(1), e2.key);
        e1.key = 0;
        priIndex1.putNoOverwrite(txn, e1);
        assertEquals(2, e1.key);
        e2.key = null;
        priIndex2.put(txn, e2);
        assertEquals(Integer.valueOf(2), e2.key);
        txnCommit(txn);

        close();
    }

    @Entity
    static class SeparateSequenceEntity1 {

        @PrimaryKey(sequence="seq1")
        private int key;
    }

    @Entity
    static class SeparateSequenceEntity2 {

        @PrimaryKey(sequence="seq2")
        private Integer key;
    }
    
    public void testCompositeSequence() 
        throws DatabaseException {

        open();

        PrimaryIndex<CompositeSequenceEntity1.Key,CompositeSequenceEntity1>
            priIndex1 =
            store.getPrimaryIndex
                (CompositeSequenceEntity1.Key.class,
                 CompositeSequenceEntity1.class);

        PrimaryIndex<CompositeSequenceEntity2.Key,CompositeSequenceEntity2>
            priIndex2 =
            store.getPrimaryIndex
                (CompositeSequenceEntity2.Key.class,
                 CompositeSequenceEntity2.class);

        Transaction txn = txnBegin();
        CompositeSequenceEntity1 e1 = new CompositeSequenceEntity1();
        CompositeSequenceEntity2 e2 = new CompositeSequenceEntity2();
        priIndex1.put(txn, e1);
        assertEquals(1, e1.key.key);
        priIndex2.putNoOverwrite(txn, e2);
        assertEquals(Integer.valueOf(1), e2.key.key);
        e1.key = null;
        priIndex1.putNoOverwrite(txn, e1);
        assertEquals(2, e1.key.key);
        e2.key = null;
        priIndex2.put(txn, e2);
        assertEquals(Integer.valueOf(2), e2.key.key);
        txnCommit(txn);

        EntityCursor<CompositeSequenceEntity1> c1 = priIndex1.entities();
        e1 = c1.next();
        assertEquals(2, e1.key.key);
        e1 = c1.next();
        assertEquals(1, e1.key.key);
        e1 = c1.next();
        assertNull(e1);
        c1.close();

        EntityCursor<CompositeSequenceEntity2> c2 = priIndex2.entities();
        e2 = c2.next();
        assertEquals(Integer.valueOf(2), e2.key.key);
        e2 = c2.next();
        assertEquals(Integer.valueOf(1), e2.key.key);
        e2 = c2.next();
        assertNull(e2);
        c2.close();

        close();
    }

    @Entity
    static class CompositeSequenceEntity1 {

        @Persistent
        static class Key implements Comparable<Key> {

            @KeyField(1)
            private int key;

            public int compareTo(Key o) {
                /* Reverse the natural order. */
                return o.key - key;
            }
        }

        @PrimaryKey(sequence="seq1")
        private Key key;
    }

    @Entity
    static class CompositeSequenceEntity2 {

        @Persistent
        static class Key implements Comparable<Key> {

            @KeyField(1)
            private Integer key;

            public int compareTo(Key o) {
                /* Reverse the natural order. */
                return o.key - key;
            }
        }

        @PrimaryKey(sequence="seq2")
        private Key key;
    }

    /**
     * When opening read-only, secondaries are not opened when the primary is
     * opened, causing a different code path to be used for opening
     * secondaries.  For a RawStore in particular, this caused an unreported
     * NullPointerException in JE 3.0.12.  No SR was created because the use
     * case is very obscure and was discovered by code inspection.
     */
    public void testOpenRawStoreReadOnly()
        throws DatabaseException {

        open();
        store.getPrimaryIndex(Integer.class, MyEntity.class);
        close();

        StoreConfig config = new StoreConfig();
        config.setReadOnly(true);
        config.setTransactional(envConfig.getTransactional());
        RawStore rawStore = new RawStore(env, "test", config);

        String clsName = MyEntity.class.getName();
        rawStore.getSecondaryIndex(clsName, "secKey");

        rawStore.close();
    }

    /**
     * When opening an X_TO_MANY secondary that has a persistent key class, the
     * key class was not recognized as being persistent if it was never before
     * referenced when getSecondaryIndex was called.  This was a bug in JE
     * 3.0.12, reported on OTN.  [#15103]
     */
    public void testToManyKeyClass()
        throws DatabaseException {

        open();

        PrimaryIndex<Integer,ToManyKeyEntity> priIndex =
            store.getPrimaryIndex(Integer.class, ToManyKeyEntity.class);
        SecondaryIndex<ToManyKey,Integer,ToManyKeyEntity> secIndex =
            store.getSecondaryIndex(priIndex, ToManyKey.class, "key2");

        priIndex.put(new ToManyKeyEntity());
        secIndex.get(new ToManyKey());

        close();
    }

    /**
     * Test a fix for a bug where opening a TO_MANY secondary index would fail
     * fail with "IllegalArgumentException: Wrong secondary key class: ..."
     * when the store was opened read-only.  [#15156]
     */
    public void testToManyReadOnly()
        throws DatabaseException {

        open();
        PrimaryIndex<Integer,ToManyKeyEntity> priIndex =
            store.getPrimaryIndex(Integer.class, ToManyKeyEntity.class);
        priIndex.put(new ToManyKeyEntity());
        close();

        openReadOnly();
        priIndex = store.getPrimaryIndex(Integer.class, ToManyKeyEntity.class);
        SecondaryIndex<ToManyKey,Integer,ToManyKeyEntity> secIndex =
            store.getSecondaryIndex(priIndex, ToManyKey.class, "key2");
        secIndex.get(new ToManyKey());
        close();
    }

    @Persistent
    static class ToManyKey {

        @KeyField(1)
        int value = 99;
    }

    @Entity
    static class ToManyKeyEntity {

        @PrimaryKey
        int key = 88;

        @SecondaryKey(relate=ONE_TO_MANY)
        Set<ToManyKey> key2;

        ToManyKeyEntity() {
            key2 = new HashSet<ToManyKey>();
            key2.add(new ToManyKey());
        }
    }


    /**
     * When Y is opened and X has a key with relatedEntity=Y.class, X should
     * be opened automatically.  If X is not opened, foreign key constraints
     * will not be enforced. [#15358]
     */
    public void testAutoOpenRelatedEntity()
        throws DatabaseException {

        PrimaryIndex<Integer,RelatedY> priY;
        PrimaryIndex<Integer,RelatedX> priX;

        /* Opening X should create (and open) Y and enforce constraints. */
        open();
        priX = store.getPrimaryIndex(Integer.class, RelatedX.class);
        PersistTestUtils.assertDbExists
            (true, env, STORE_NAME, RelatedY.class.getName(), null);
        try {
            priX.put(new RelatedX());
            fail();
        } catch (DatabaseException e) {
            assertTrue
                ("" + e.getMessage(), (e.getMessage().indexOf
                  ("foreign key not allowed: it is not present") >= 0) ||
                 (e.getMessage().indexOf("DB_FOREIGN_CONFLICT") >= 0));
        }
        priY = store.getPrimaryIndex(Integer.class, RelatedY.class);
        priY.put(new RelatedY());
        priX.put(new RelatedX());
        close();

        /* Delete should cascade even when X is not opened explicitly. */
        open();
        priY = store.getPrimaryIndex(Integer.class, RelatedY.class);
        assertEquals(1, priY.count());
        priY.delete(88);
        assertEquals(0, priY.count());
        priX = store.getPrimaryIndex(Integer.class, RelatedX.class);
        assertEquals(0, priX.count()); /* Failed prior to [#15358] fix. */
        close();
    }

    @Entity
    static class RelatedX {

        @PrimaryKey
        int key = 99;

        @SecondaryKey(relate=ONE_TO_ONE,
                      relatedEntity=RelatedY.class,
                      onRelatedEntityDelete=CASCADE)
        int key2 = 88;

        RelatedX() {
        }
    }

    @Entity
    static class RelatedY {

        @PrimaryKey
        int key = 88;

        RelatedY() {
        }
    }

    public void testSecondaryBulkLoad1()
        throws DatabaseException {

        doSecondaryBulkLoad(true);
    }

    public void testSecondaryBulkLoad2()
        throws DatabaseException {

        doSecondaryBulkLoad(false);
    }

    private void doSecondaryBulkLoad(boolean closeAndOpenNormally)
        throws DatabaseException {

        PrimaryIndex<Integer,RelatedX> priX;
        PrimaryIndex<Integer,RelatedY> priY;
        SecondaryIndex<Integer,Integer,RelatedX> secX;

        /* Open priX with SecondaryBulkLoad=true. */
        StoreConfig config = new StoreConfig();
        config.setAllowCreate(true);
        config.setSecondaryBulkLoad(true);
        open(config);

        /* Getting priX should not create the secondary index. */
        priX = store.getPrimaryIndex(Integer.class, RelatedX.class);
        PersistTestUtils.assertDbExists
            (false, env, STORE_NAME, RelatedX.class.getName(), "key2");

        /* We can put records that violate the secondary key constraint. */
        priX.put(new RelatedX());

        if (closeAndOpenNormally) {
            /* Open normally and the secondary will be populated. */
            close();
            open();
            try {
                /* Before adding the foreign key, constraint is violated. */
                priX = store.getPrimaryIndex(Integer.class, RelatedX.class);
            } catch (DatabaseException e) {
                assertTrue(e.toString(),
                           e.toString().contains("foreign key not allowed"));
            }
            /* Add the foreign key to avoid the constraint error. */
            priY = store.getPrimaryIndex(Integer.class, RelatedY.class);
            priY.put(new RelatedY());
            priX = store.getPrimaryIndex(Integer.class, RelatedX.class);
            PersistTestUtils.assertDbExists
                (true, env, STORE_NAME, RelatedX.class.getName(), "key2");
            secX = store.getSecondaryIndex(priX, Integer.class, "key2");
        } else {
            /* Get secondary index explicitly and it will be populated. */
            try {
                /* Before adding the foreign key, constraint is violated. */
                secX = store.getSecondaryIndex(priX, Integer.class, "key2");
            } catch (DatabaseException e) {
                assertTrue(e.toString(),
                           e.toString().contains("foreign key not allowed"));
            }
            /* Add the foreign key. */
            priY = store.getPrimaryIndex(Integer.class, RelatedY.class);
            priY.put(new RelatedY());
            secX = store.getSecondaryIndex(priX, Integer.class, "key2");
            PersistTestUtils.assertDbExists
                (true, env, STORE_NAME, RelatedX.class.getName(), "key2");
        }

        RelatedX x = secX.get(88);
        assertNotNull(x);
        close();
    }
}
