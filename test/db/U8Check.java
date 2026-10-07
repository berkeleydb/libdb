/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later OR Sleepycat-OSL
 *
 * See the file LICENSE for redistribution information.
 */

// U8 verification: the four backup tunables must actually reach the library.
//
// Before this fix setBackupReadCount() and friends stored to a private field and
// nothing pushed the value down, so calling them had NO EFFECT. Compilation
// cannot detect that, and neither can an accessor round-trip test -- the field
// round-trips perfectly either way. The only honest check is to open a real
// environment, then read the value back THROUGH THE C LAYER and compare.
//
// DB_ENV->get_backup_config returns EINVAL unless a backup handle exists, and a
// handle exists exactly when a BackupHandler was installed, so the test installs
// a no-op one.
package com.sleepycat.db;

import com.sleepycat.db.internal.DbConstants;
import java.io.File;

public class U8Check {

    private static int fails = 0;

    private static void check(String what, boolean cond) {
        System.out.println((cond ? "  ok   " : "  FAIL ") + what);
        if (!cond)
            fails++;
    }

    /* A handler that does nothing: its presence is what allocates the handle. */
    static class NoopBackup implements BackupHandler {
        public int close(String dbname) { return 0; }
        public int open(String target, String dbname) { return 0; }
        public int write(long file_pos, byte[] buf, int off, int len) {
            return 0;
        }
    }

    public static void main(String[] args) throws Exception {
        File home = new File(args.length > 0 ? args[0] : "/tmp/U8DIR");
        home.mkdirs();

        EnvironmentConfig ec = new EnvironmentConfig();
        ec.setAllowCreate(true);
        ec.setInitializeCache(true);
        ec.setTransactional(true);
        ec.setInitializeLogging(true);
        ec.setInitializeLocking(true);
        ec.setBackupHandler(new NoopBackup());

        /* Values deliberately unlike any default. */
        ec.setBackupReadCount(4096);
        ec.setBackupReadSleep(1234);
        ec.setBackupSize(65536);
        ec.setBackupWriteDirect(true);

        Environment env = new Environment(home, ec);

        /*
         * Read back through the C layer, NOT through the Java field: a
         * fresh EnvironmentConfig from the live handle.
         */
        EnvironmentConfig got = env.getConfig();
        check("read_count reached the library (4096, got " +
            got.getBackupReadCount() + ")", got.getBackupReadCount() == 4096);
        check("read_sleep reached the library (1234, got " +
            got.getBackupReadSleep() + ")", got.getBackupReadSleep() == 1234);
        check("size reached the library (65536, got " +
            got.getBackupSize() + ")", got.getBackupSize() == 65536);
        check("write_direct reached the library (true, got " +
            got.getBackupWriteDirect() + ")", got.getBackupWriteDirect());

        env.close();

        System.out.println(fails == 0
            ? "VERDICT u8 PASS the four backup tunables reach the C layer"
            : "VERDICT u8 FAIL " + fails + " tunable(s) did not reach the library");
        System.exit(fails == 0 ? 0 : 1);
    }
}
