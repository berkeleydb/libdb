# DO NOT EDIT: automatically built by dist/s_android.
# Makefile for building a drop-in replacement of SQLite using
# Berkeley DB 11g Release 2, library version 11.2.5.0.32: (October 25, 2010)
###################################################################
LOCAL_PATH := $(call my-dir)
include $(CLEAR_VARS)

###################################################################
# build libsqlite replacement
LOCAL_MODULE := libsqlite

# BDB_TOP will change with release numbers
BDB_TOP := db-5.0.32
BDB_PATH := $(LOCAL_PATH)/$(BDB_TOP)

# This directive results in arm (vs thumb) code.  It's necessary to
# allow some BDB assembler code (for mutexes) to compile.
LOCAL_ARM_MODE := arm

# basic includes for BDB 11gR2
LOCAL_C_INCLUDES := $(BDB_PATH) $(LOCAL_PATH)/$(BDB_TOP)/build_android \
	$(LOCAL_PATH)/$(BDB_TOP)/sql/generated

# this is needed for sqlite3.c
LOCAL_C_INCLUDES += $(LOCAL_PATH)/$(BDB_TOP)/build_android/sql

# Source files
LOCAL_SRC_FILES := \
-e 	$(BDB_TOP)/btree/bt_compact.c \
-e 	$(BDB_TOP)/btree/bt_compare.c \
-e 	$(BDB_TOP)/btree/bt_compress.c \
-e 	$(BDB_TOP)/btree/bt_conv.c \
-e 	$(BDB_TOP)/btree/bt_curadj.c \
-e 	$(BDB_TOP)/btree/bt_cursor.c \
-e 	$(BDB_TOP)/btree/bt_delete.c \
-e 	$(BDB_TOP)/btree/bt_method.c \
-e 	$(BDB_TOP)/btree/bt_open.c \
-e 	$(BDB_TOP)/btree/bt_put.c \
-e 	$(BDB_TOP)/btree/bt_rec.c \
-e 	$(BDB_TOP)/btree/bt_reclaim.c \
-e 	$(BDB_TOP)/btree/bt_recno.c \
-e 	$(BDB_TOP)/btree/bt_rsearch.c \
-e 	$(BDB_TOP)/btree/bt_search.c \
-e 	$(BDB_TOP)/btree/bt_split.c \
-e 	$(BDB_TOP)/btree/bt_stat.c \
-e 	$(BDB_TOP)/btree/bt_upgrade.c \
-e 	$(BDB_TOP)/btree/btree_auto.c \
-e 	$(BDB_TOP)/clib/rand.c \
-e 	$(BDB_TOP)/clib/snprintf.c \
-e 	$(BDB_TOP)/common/clock.c \
-e 	$(BDB_TOP)/common/crypto_stub.c \
-e 	$(BDB_TOP)/common/db_byteorder.c \
-e 	$(BDB_TOP)/common/db_compint.c \
-e 	$(BDB_TOP)/common/db_err.c \
-e 	$(BDB_TOP)/common/db_getlong.c \
-e 	$(BDB_TOP)/common/db_idspace.c \
-e 	$(BDB_TOP)/common/db_log2.c \
-e 	$(BDB_TOP)/common/db_shash.c \
-e 	$(BDB_TOP)/common/dbt.c \
-e 	$(BDB_TOP)/common/mkpath.c \
-e 	$(BDB_TOP)/common/os_method.c \
-e 	$(BDB_TOP)/common/zerofill.c \
-e 	$(BDB_TOP)/db/crdel_auto.c \
-e 	$(BDB_TOP)/db/crdel_rec.c \
-e 	$(BDB_TOP)/db/db.c \
-e 	$(BDB_TOP)/db/db_am.c \
-e 	$(BDB_TOP)/db/db_auto.c \
-e 	$(BDB_TOP)/db/db_cam.c \
-e 	$(BDB_TOP)/db/db_cds.c \
-e 	$(BDB_TOP)/db/db_compact.c \
-e 	$(BDB_TOP)/db/db_conv.c \
-e 	$(BDB_TOP)/db/db_dispatch.c \
-e 	$(BDB_TOP)/db/db_dup.c \
-e 	$(BDB_TOP)/db/db_iface.c \
-e 	$(BDB_TOP)/db/db_join.c \
-e 	$(BDB_TOP)/db/db_meta.c \
-e 	$(BDB_TOP)/db/db_method.c \
-e 	$(BDB_TOP)/db/db_open.c \
-e 	$(BDB_TOP)/db/db_overflow.c \
-e 	$(BDB_TOP)/db/db_pr.c \
-e 	$(BDB_TOP)/db/db_rec.c \
-e 	$(BDB_TOP)/db/db_reclaim.c \
-e 	$(BDB_TOP)/db/db_remove.c \
-e 	$(BDB_TOP)/db/db_rename.c \
-e 	$(BDB_TOP)/db/db_ret.c \
-e 	$(BDB_TOP)/db/db_setid.c \
-e 	$(BDB_TOP)/db/db_setlsn.c \
-e 	$(BDB_TOP)/db/db_sort_multiple.c \
-e 	$(BDB_TOP)/db/db_stati.c \
-e 	$(BDB_TOP)/db/db_truncate.c \
-e 	$(BDB_TOP)/db/db_upg.c \
-e 	$(BDB_TOP)/db/db_upg_opd.c \
-e 	$(BDB_TOP)/db/db_vrfy_stub.c \
-e 	$(BDB_TOP)/db/partition.c \
-e 	$(BDB_TOP)/dbreg/dbreg.c \
-e 	$(BDB_TOP)/dbreg/dbreg_auto.c \
-e 	$(BDB_TOP)/dbreg/dbreg_rec.c \
-e 	$(BDB_TOP)/dbreg/dbreg_stat.c \
-e 	$(BDB_TOP)/dbreg/dbreg_util.c \
-e 	$(BDB_TOP)/env/env_alloc.c \
-e 	$(BDB_TOP)/env/env_config.c \
-e 	$(BDB_TOP)/env/env_failchk.c \
-e 	$(BDB_TOP)/env/env_file.c \
-e 	$(BDB_TOP)/env/env_globals.c \
-e 	$(BDB_TOP)/env/env_method.c \
-e 	$(BDB_TOP)/env/env_name.c \
-e 	$(BDB_TOP)/env/env_open.c \
-e 	$(BDB_TOP)/env/env_recover.c \
-e 	$(BDB_TOP)/env/env_region.c \
-e 	$(BDB_TOP)/env/env_register.c \
-e 	$(BDB_TOP)/env/env_sig.c \
-e 	$(BDB_TOP)/env/env_stat.c \
-e 	$(BDB_TOP)/fileops/fileops_auto.c \
-e 	$(BDB_TOP)/fileops/fop_basic.c \
-e 	$(BDB_TOP)/fileops/fop_rec.c \
-e 	$(BDB_TOP)/fileops/fop_util.c \
-e 	$(BDB_TOP)/hash/hash_func.c \
-e 	$(BDB_TOP)/hash/hash_stub.c \
-e 	$(BDB_TOP)/hmac/hmac.c \
-e 	$(BDB_TOP)/hmac/sha1.c \
-e 	$(BDB_TOP)/lock/lock.c \
-e 	$(BDB_TOP)/lock/lock_deadlock.c \
-e 	$(BDB_TOP)/lock/lock_failchk.c \
-e 	$(BDB_TOP)/lock/lock_id.c \
-e 	$(BDB_TOP)/lock/lock_list.c \
-e 	$(BDB_TOP)/lock/lock_method.c \
-e 	$(BDB_TOP)/lock/lock_region.c \
-e 	$(BDB_TOP)/lock/lock_stat.c \
-e 	$(BDB_TOP)/lock/lock_timer.c \
-e 	$(BDB_TOP)/lock/lock_util.c \
-e 	$(BDB_TOP)/log/log.c \
-e 	$(BDB_TOP)/log/log_archive.c \
-e 	$(BDB_TOP)/log/log_compare.c \
-e 	$(BDB_TOP)/log/log_debug.c \
-e 	$(BDB_TOP)/log/log_get.c \
-e 	$(BDB_TOP)/log/log_method.c \
-e 	$(BDB_TOP)/log/log_print.c \
-e 	$(BDB_TOP)/log/log_put.c \
-e 	$(BDB_TOP)/log/log_stat.c \
-e 	$(BDB_TOP)/log/log_verify_stub.c \
-e 	$(BDB_TOP)/mp/mp_alloc.c \
-e 	$(BDB_TOP)/mp/mp_bh.c \
-e 	$(BDB_TOP)/mp/mp_fget.c \
-e 	$(BDB_TOP)/mp/mp_fmethod.c \
-e 	$(BDB_TOP)/mp/mp_fopen.c \
-e 	$(BDB_TOP)/mp/mp_fput.c \
-e 	$(BDB_TOP)/mp/mp_fset.c \
-e 	$(BDB_TOP)/mp/mp_method.c \
-e 	$(BDB_TOP)/mp/mp_mvcc.c \
-e 	$(BDB_TOP)/mp/mp_region.c \
-e 	$(BDB_TOP)/mp/mp_register.c \
-e 	$(BDB_TOP)/mp/mp_resize.c \
-e 	$(BDB_TOP)/mp/mp_stat.c \
-e 	$(BDB_TOP)/mp/mp_sync.c \
-e 	$(BDB_TOP)/mp/mp_trickle.c \
-e 	$(BDB_TOP)/mutex/mut_alloc.c \
-e 	$(BDB_TOP)/mutex/mut_failchk.c \
-e 	$(BDB_TOP)/mutex/mut_method.c \
-e 	$(BDB_TOP)/mutex/mut_region.c \
-e 	$(BDB_TOP)/mutex/mut_stat.c \
-e 	$(BDB_TOP)/mutex/mut_tas.c \
-e 	$(BDB_TOP)/os/os_abort.c \
-e 	$(BDB_TOP)/os/os_abs.c \
-e 	$(BDB_TOP)/os/os_alloc.c \
-e 	$(BDB_TOP)/os/os_clock.c \
-e 	$(BDB_TOP)/os/os_config.c \
-e 	$(BDB_TOP)/os/os_cpu.c \
-e 	$(BDB_TOP)/os/os_ctime.c \
-e 	$(BDB_TOP)/os/os_dir.c \
-e 	$(BDB_TOP)/os/os_errno.c \
-e 	$(BDB_TOP)/os/os_fid.c \
-e 	$(BDB_TOP)/os/os_flock.c \
-e 	$(BDB_TOP)/os/os_fsync.c \
-e 	$(BDB_TOP)/os/os_getenv.c \
-e 	$(BDB_TOP)/os/os_handle.c \
-e 	$(BDB_TOP)/os/os_map.c \
-e 	$(BDB_TOP)/os/os_mkdir.c \
-e 	$(BDB_TOP)/os/os_open.c \
-e 	$(BDB_TOP)/os/os_pid.c \
-e 	$(BDB_TOP)/os/os_rename.c \
-e 	$(BDB_TOP)/os/os_root.c \
-e 	$(BDB_TOP)/os/os_rpath.c \
-e 	$(BDB_TOP)/os/os_rw.c \
-e 	$(BDB_TOP)/os/os_seek.c \
-e 	$(BDB_TOP)/os/os_stack.c \
-e 	$(BDB_TOP)/os/os_stat.c \
-e 	$(BDB_TOP)/os/os_tmpdir.c \
-e 	$(BDB_TOP)/os/os_truncate.c \
-e 	$(BDB_TOP)/os/os_uid.c \
-e 	$(BDB_TOP)/os/os_unlink.c \
-e 	$(BDB_TOP)/os/os_yield.c \
-e 	$(BDB_TOP)/qam/qam_stub.c \
-e 	$(BDB_TOP)/rep/rep_stub.c \
-e 	$(BDB_TOP)/repmgr/repmgr_stub.c \
-e 	$(BDB_TOP)/sequence/seq_stat.c \
-e 	$(BDB_TOP)/sequence/sequence.c \
-e 	$(BDB_TOP)/txn/txn.c \
-e 	$(BDB_TOP)/txn/txn_auto.c \
-e 	$(BDB_TOP)/txn/txn_chkpt.c \
-e 	$(BDB_TOP)/txn/txn_failchk.c \
-e 	$(BDB_TOP)/txn/txn_method.c \
-e 	$(BDB_TOP)/txn/txn_rec.c \
-e 	$(BDB_TOP)/txn/txn_recover.c \
-e 	$(BDB_TOP)/txn/txn_region.c \
-e 	$(BDB_TOP)/txn/txn_stat.c \
-e 	$(BDB_TOP)/txn/txn_util.c \
	$(BDB_TOP)/sql/generated/sqlite3.c

ifneq ($(TARGET_ARCH),arm)
LOCAL_LDLIBS += -lpthread -ldl
endif

# flags -- most of these are from the SQLite build, some are not.
# SQLITE_DEFAULT_JOURNAL_SIZE_LIMIT controls the default size of
# the Berkeley DB log file (set to 1MB here).
LOCAL_CFLAGS += -Wall -DHAVE_USLEEP=1 \
	-DSQLITE_DEFAULT_JOURNAL_SIZE_LIMIT=1048576 \
	-DSQLITE_THREADSAFE=1 -DNDEBUG=1 -DSQLITE_TEMP_STORE=3 \
	-DSQLITE_OMIT_TRUNCATE_OPTIMIZATION -DSQLITE_OS_UNIX=1 \
	-D_HAVE_SQLITE_CONFIG_H -DSQLITE_THREAD_OVERRIDE_LOCK=-1 \
	-DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS3_BACKWARDS -Dfdatasync=fsync

# LOCAL_CFLAGS that are not used at this time that should be examined
# -DSQLITE_ENABLE_POISON
# -DSQLITE_ENABLE_MEMORY_MANAGEMENT

ifneq ($(TARGET_SIMULATOR),true)
LOCAL_SHARED_LIBRARIES := libdl
endif

LOCAL_C_INCLUDES += $(call include-path-for, system-core)/cutils
LOCAL_SHARED_LIBRARIES += liblog libicuuc libicui18n libutils

# This links in some static symbols from Android
LOCAL_WHOLE_STATIC_LIBRARIES := libsqlite3_android

include $(BUILD_SHARED_LIBRARY)

################################################################################
##device commande line tool:sqlite3
################################################################################
ifneq ($(SDK_ONLY),true)  # SDK doesn't need device version of sqlite3
include $(CLEAR_VARS)

LOCAL_ARM_MODE := arm
LOCAL_SRC_FILES := $(BDB_TOP)/sql/sqlite/src/shell.c
LOCAL_SHARED_LIBRARIES := libsqlite
LOCAL_C_INCLUDES := $(BDB_PATH) $(LOCAL_PATH)/$(BDB_TOP)/build_android\
	 $(LOCAL_PATH)/$(BDB_TOP)/sql/generated $(LOCAL_PATH)/../android

ifneq ($(TARGET_ARCH),arm)
LOCAL_LDLIBS += -lpthread -ldl
endif

LOCAL_CFLAGS += -DHAVE_USLEEP=1 -DTHREADSAFE=1 -DNDEBUG=1
LOCAL_MODULE_PATH := $(TARGET_OUT_OPTIONAL_EXECUTABLES)
LOCAL_MODULE_TAGS := debug
LOCAL_MODULE := sqlite3
include $(BUILD_EXECUTABLE)
endif # !SDK_ONLY

