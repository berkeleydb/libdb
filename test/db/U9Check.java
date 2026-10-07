/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later OR Sleepycat-OSL
 *
 * See the file LICENSE for redistribution information.
 */

// U9 verification: the serializable accessors added to TransactionConfig,
// CursorConfig and EnvironmentConfig must round-trip, must be independent of the
// snapshot switch, and must be wired to DbConstants.DB_TXN_SERIALIZABLE in the
// flag words the C layer receives.
//
// The flag words are computed inside beginTransaction()/openCursor(), which need
// a live environment, so this checks two things separately: the accessor/field
// contract by reflection, and that each class's flag-assembly code actually
// references DB_TXN_SERIALIZABLE (asserted by the caller grepping the source).
// Lives in the package so it can see the private fields.
package com.sleepycat.db;

import com.sleepycat.db.internal.DbConstants;
import java.lang.reflect.Field;

public class U9Check {

    private static int fails = 0;

    private static boolean field(Object o, String name) throws Exception {
        Field f = o.getClass().getDeclaredField(name);
        f.setAccessible(true);
        return f.getBoolean(o);
    }

    private static void check(String what, boolean cond) {
        System.out.println((cond ? "  ok   " : "  FAIL ") + what);
        if (!cond)
            fails++;
    }

    public static void main(String[] args) throws Exception {
        check("DB_TXN_SERIALIZABLE is 0x00200000",
            DbConstants.DB_TXN_SERIALIZABLE == 0x00200000);
        check("DB_TXN_SERIALIZABLE differs from DB_TXN_SNAPSHOT",
            DbConstants.DB_TXN_SERIALIZABLE != DbConstants.DB_TXN_SNAPSHOT);

        // --- TransactionConfig ---
        TransactionConfig t = new TransactionConfig();
        check("TransactionConfig default serializable=false",
            !t.getSerializable() && !field(t, "serializable"));
        t.setSerializable(true);
        check("TransactionConfig setSerializable(true) round-trips",
            t.getSerializable() && field(t, "serializable"));
        check("TransactionConfig serializable leaves snapshot alone",
            !t.getSnapshot());
        t.setSnapshot(true);
        check("TransactionConfig snapshot+serializable are independent",
            t.getSnapshot() && t.getSerializable());
        t.setSerializable(false);
        check("TransactionConfig setSerializable(false) clears, snapshot kept",
            !t.getSerializable() && t.getSnapshot());

        // --- CursorConfig ---
        CursorConfig c = new CursorConfig();
        check("CursorConfig default serializable=false",
            !c.getSerializable() && !field(c, "serializable"));
        c.setSerializable(true);
        check("CursorConfig setSerializable(true) round-trips",
            c.getSerializable() && field(c, "serializable"));
        check("CursorConfig.SERIALIZABLE convenience instance is configured",
            CursorConfig.SERIALIZABLE.getSerializable());
        check("CursorConfig.SNAPSHOT unchanged by the addition",
            CursorConfig.SNAPSHOT.getSnapshot()
            && !CursorConfig.SNAPSHOT.getSerializable());
        check("CursorConfig.DEFAULT has neither",
            !CursorConfig.DEFAULT.getSnapshot()
            && !CursorConfig.DEFAULT.getSerializable());

        // --- EnvironmentConfig ---
        EnvironmentConfig e = new EnvironmentConfig();
        check("EnvironmentConfig default txnSerializable=false",
            !e.getTxnSerializable() && !field(e, "txnSerializable"));
        e.setTxnSerializable(true);
        check("EnvironmentConfig setTxnSerializable(true) round-trips",
            e.getTxnSerializable() && field(e, "txnSerializable"));
        check("EnvironmentConfig txnSnapshot is independent",
            !e.getTxnSnapshot());

        System.out.println(fails == 0
            ? "VERDICT u9 PASS all checks"
            : "VERDICT u9 FAIL " + fails + " check(s)");
        System.exit(fails == 0 ? 0 : 1);
    }
}
