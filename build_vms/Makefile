# @(#)Makefile	10.2 (Sleepycat) 11/2/98
#

srcdir=	../dist/..

#################################################################
# VMS build
#

NOOPT=1

%ifdef NODEBUG
DEBUG = /NOdebug
%else
DEBUG = /debug
%endif

%ifdef NOOPT
OPT = /NOoptimize
%else
OPT = /optimize
%endif

%ifdef LIST
LIST = /list/show=all
%else
LIST = /NOlist
%endif

%ifndef CEXTRA
CEXTRA =
%endif

%ifdef LDEBUG
LDEBUG = /debug
%else
LDEBUG = /NOdebug
%endif

%ifdef MAP
MAP = /map/full
%else
MAP = /NOmap
%endif

C_GLOBAL_DEFS = __KVC_NOOP
###C_GLOBAL_DEFS = __CRTL_VER=60100000,__VMS_VER=60100000 ###for VMS V6

DEFS = /define=($(C_GLOBAL_DEFS))

INCLUDES= /include=([],[-.include])
###INCLUDES= /include=([],[-.include],dskb:[crw.src.tcl]) ### for TCL

CFLAGS = /prefix=all/float=g_float/extern=relaxed_refdef/noshare \
         /member_alignment \
         $(LIST)$(DEBUG)$(OPT)$(DEFS)$(INCLUDES)$(CEXTRA)
CC     = cc/decc
CXX    = cxx

LFLAGS = $(LDEBUG) $(MAP)
LINK = link
#
#
#################################################################

SHELL=	/usr/bin/sh
ar=	/usr/bin/ar cq
chmod=	/usr/bin/chmod
cp=	/usr/bin/cp
mkdir=	/usr/bin/mkdir
ranlib= /usr/bin/ranlib
rm=	/usr/bin/rm
strip=	/usr/bin/strip

libdb=	libdb.olb

OBJS=	$(libdb)(bt_compare), \
	$(libdb)(bt_conv), \
	$(libdb)(bt_curadj), \
	$(libdb)(bt_cursor), \
	$(libdb)(bt_delete), \
	$(libdb)(bt_open), \
	$(libdb)(bt_page), \
	$(libdb)(bt_put), \
	$(libdb)(bt_rec), \
	$(libdb)(bt_recno), \
	$(libdb)(bt_rsearch), \
	$(libdb)(bt_search), \
	$(libdb)(bt_split), \
	$(libdb)(bt_stat), \
	$(libdb)(btree_auto), \
	$(libdb)(db), \
	$(libdb)(db_am), \
	$(libdb)(db_appinit), \
	$(libdb)(db_apprec), \
	$(libdb)(db_auto), \
	$(libdb)(db_byteorder), \
	$(libdb)(db_conv), \
	$(libdb)(db_dispatch), \
	$(libdb)(db_dup), \
	$(libdb)(db_err), \
	$(libdb)(db_iface), \
	$(libdb)(db_join), \
	$(libdb)(db_log2), \
	$(libdb)(db_overflow), \
	$(libdb)(db_pr), \
	$(libdb)(db_rec), \
	$(libdb)(db_region), \
	$(libdb)(db_ret), \
	$(libdb)(db_salloc), \
	$(libdb)(db_shash), \
	$(libdb)(dbm), \
	$(libdb)(hash), \
	$(libdb)(hash_auto), \
	$(libdb)(hash_conv), \
	$(libdb)(hash_dup), \
	$(libdb)(hash_func), \
	$(libdb)(hash_page), \
	$(libdb)(hash_rec), \
	$(libdb)(hash_stat), \
	$(libdb)(hsearch), \
	$(libdb)(lock), \
	$(libdb)(lock_conflict), \
	$(libdb)(lock_deadlock), \
	$(libdb)(lock_region), \
	$(libdb)(lock_util), \
	$(libdb)(log), \
	$(libdb)(log_archive), \
	$(libdb)(log_auto), \
	$(libdb)(log_compare), \
	$(libdb)(log_findckp), \
	$(libdb)(log_get), \
	$(libdb)(log_put), \
	$(libdb)(log_rec), \
	$(libdb)(log_register), \
	$(libdb)(mp_bh), \
	$(libdb)(mp_fget), \
	$(libdb)(mp_fopen), \
	$(libdb)(mp_fput), \
	$(libdb)(mp_fset), \
	$(libdb)(mp_open), \
	$(libdb)(mp_pr), \
	$(libdb)(mp_region), \
	$(libdb)(mp_sync), \
	$(libdb)(mutex), \
	$(libdb)(os_abs), \
	$(libdb)(os_alloc), \
	$(libdb)(os_config), \
	$(libdb)(os_dir), \
	$(libdb)(os_fid), \
	$(libdb)(os_fsync), \
	$(libdb)(os_map), \
	$(libdb)(os_oflags), \
	$(libdb)(os_open), \
	$(libdb)(os_rpath), \
	$(libdb)(os_rw), \
	$(libdb)(os_seek), \
	$(libdb)(os_sleep), \
	$(libdb)(os_spin), \
	$(libdb)(os_stat), \
	$(libdb)(os_tmpdir), \
	$(libdb)(os_unlink), \
	$(libdb)(txn), \
	$(libdb)(txn_auto), \
	$(libdb)(txn_rec)

COBJS=	cxx_app.obj, \
	cxx_except.obj, \
	cxx_lock.obj, \
	cxx_log.obj, \
	cxx_mpool.obj, \
	cxx_table.obj, \
	cxx_txn.obj

PROGS=	db_archive.exe db_checkpoint.exe db_deadlock.exe db_dump.exe \
	db_load.exe db_recover.exe db_stat.exe

LOBJS=	 $(libdb)(snprintf), $(libdb)(vsnprintf)
LLIBS=
POBJS=	err.obj, getlong.obj

LIBOBJS= $(OBJS) $(LOBJS)

TCLLIB=
##TCLLIB = dskb:[alpha_obj]tcl.olb/lib     ###for TCL

all: $(PROGS)

$(libdb):
        if F$SEARCH("$(libdb)") .eqs. "" then libr/create $(libdb)

DBA=	db_archive.obj, $(POBJS)
db_archive.exe: $(DBA) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBA), $(libdb)/lib $(LLIBS)

DBB=	db_checkpoint.obj, $(POBJS)
db_checkpoint.exe: $(DBB) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBB), $(libdb)/lib $(LLIBS)

DBC=	db_deadlock.obj, $(POBJS)
db_deadlock.exe: $(DBC) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBC), $(libdb)/lib $(LLIBS)

DBD=	db_dump.obj, $(POBJS)
db_dump.exe: $(DBD) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBD), $(libdb)/lib $(LLIBS)

DBE=	db_dump185.obj, $(POBJS)
db_dump185.exe: $(DBE)
	$(LINK) $(LFLAGS) /exe=$@ $(DBE), $(LLIBS)

DBF=	db_load.obj, $(POBJS)
db_load.exe: $(DBF) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBF), $(libdb)/lib $(LLIBS)

DBG=	db_printlog.obj, $(POBJS)
db_printlog.exe: $(DBG) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBG), $(libdb)/lib $(LLIBS)

DBH=	db_recover.obj, $(POBJS)
db_recover.exe: $(DBH) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBH), $(libdb)/lib $(LLIBS)

DBI=	db_stat.obj, $(POBJS)
db_stat.exe: $(DBI) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBI), $(libdb)/lib $(LLIBS)

EPROGS=	ex_access.exe ex_appinit.exe ex_btrec.exe ex_lock.exe ex_mpool.exe \
	ex_tpcb
DBJ=	ex_access.obj,
ex_access.exe: $(DBJ) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBJ), $(libdb)/lib $(LLIBS)

DBK=	ex_appinit.obj,
ex_appinit.exe: $(DBK) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBK), $(libdb)/lib $(LLIBS)

DBL=	ex_btrec.obj,
ex_btrec.exe: $(DBL) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBL), $(libdb)/lib $(LLIBS)

DBM=	ex_lock.obj, $(POBJS)
ex_lock.exe: $(DBM) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBM), $(libdb)/lib $(LLIBS)

DBN=	ex_mpool.obj,
ex_mpool.exe: $(DBN) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBN), $(libdb)/lib $(LLIBS)

DBO=	ex_thread.obj,
ex_thread.exe: $(DBO) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBO), $(libdb)/lib $(LLIBS)

DBP=	ex_tpcb.obj, $(POBJS)
ex_tpcb.exe: $(DBP) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBP), $(libdb)/lib $(LLIBS)

DBT=	tclAppInit.obj, tcl_dbm.obj, tcl_hsearch.obj, tcl_lock.obj, \
	tcl_log.obj, tcl_mpool.obj, tcl_mutex.obj, tcl_ndbm.obj, \
	tcl_txn.obj, utils.obj, $(POBJS)

dbtest.exe: .dbtestrc
dbtest.exe: $(DBT) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBT), $(libdb)/lib, $(TCLLIB) $(LLIBS)

DBTT=	thread.obj, $(POBJS)
thread.exe:	$(DBTT) $(LIBOBJS)
	$(LINK) $(LFLAGS) /exe=$@ $(DBTT), $(libdb)/lib $(LLIBS)

##clean:
##	$(rm) -f $(OBJS)
##	$(rm) -f $(DBA) $(DBB) $(DBC) $(DBD) $(DBE) $(DBF) $(DBG) $(DBH)
##	$(rm) -f $(DBI) $(DBJ) $(DBK) $(DBL) $(DBM) $(DBN) $(DBO) $(DBP)
##	$(rm) -f $(DBT) $(DBTT)
##	$(rm) -f $(PROGS) $(EPROGS) $(POBJS) $(LOBJS)
##	$(rm) -f thread core *.core .dbtestrc $(libdb)
clean:
	delete/log .dbtestrc;*,*.obj;*,*.olb;*,*.exe;*

depend obj:

realclean distclean: clean
	$(rm) -f Makefile config.cache config.h config.log config.status
	$(rm) -f confdefs.h db.h db_int.h db_185.h include.tcl

prefix=	/usr/local
sprefix=/usr/local
exec_prefix=${prefix}
bindir=	${exec_prefix}/bin
datadir=${prefix}/share
includedir=${prefix}/include
libdir=	${exec_prefix}/lib
mandir=	${prefix}/man

dmode=	755
emode=	555
fmode=	444

transform=s,x,x,

man1=	db_archive.1 db_checkpoint.1 db_deadlock.1 db_dump.1 db_load.1 \
	db_recover.1 db_stat.1
man10=	db_archive.0 db_checkpoint.0 db_deadlock.0 db_dump.0 db_load.0 \
	db_recover.0 db_stat.0
man3=	db_appinit.3 db_cursor.3 db_dbm.3 db_dbt.3 db_hsearch.3 db_internal.3 \
	db_intro.3 db_lock.3 db_log.3 db_mpool.3 db_open.3 db_thread.3 db_txn.3
manx3=	Db.3 DbEnv.3 DbException.3 DbInfo.3 DbLock.3 DbLockTab.3 DbLog.3 \
	DbLsn.3 DbMpool.3 DbMpoolFile.3 DbTxn.3 DbTxnMgr.3 Dbc.3 Dbt.3
man30=	db_appinit.0 db_cursor.0 db_dbm.0 db_dbt.0 db_hsearch.0 db_internal.0 \
	db_intro.0 db_lock.0 db_log.0 db_mpool.0 db_open.0 db_thread.0 db_txn.0
manx30=	Db.0 DbEnv.0 DbException.0 DbInfo.0 DbLock.0 DbLockTab.0 DbLog.0 \
	DbLsn.0 DbMpool.0 DbMpoolFile.0 DbTxn.0 DbTxnMgr.0 Dbc.0 Dbt.0

install: all
	@test -f $(chmod) || (echo 'chmod not found.'; exit 1)
	@test -f $(cp) || (echo 'cp not found.'; exit 1)
	@test -f $(mkdir) || (echo 'mkdir not found.'; exit 1)
	@test -f $(rm) || (echo 'rm not found.'; exit 1)
	@test -d $(prefix) || \
	    ($(mkdir) $(prefix) && $(chmod) $(dmode) $(prefix))
	@test -d $(exec_prefix) || \
	    ($(mkdir) $(exec_prefix) && $(chmod) $(dmode) $(exec_prefix))
	@echo "Installing DB include file: $(includedir) ..."
	@test -d $(includedir) || \
	    ($(mkdir) $(includedir) && $(chmod) $(dmode) $(includedir))
	@cd $(includedir) && $(rm) -f db.h db_185.h db_cxx.h
	@$(cp) -p db.h db_185.h $(srcdir)/include/db_cxx.h $(includedir)
	@cd $(includedir) && $(chmod) $(fmode) db.h db_185.h db_cxx.h
	@echo "Installing DB library: $(libdir) ..."
	@test -d $(libdir) || \
	    ($(mkdir) $(libdir) && $(chmod) $(dmode) $(libdir))
	@cd $(libdir) && $(rm) -f $(libdb)
	@$(cp) -p $(libdb) $(libdir)
	@cd $(libdir) && $(chmod) $(fmode) $(libdb)
	@echo "Installing DB utilities: $(bindir) ..."
	@test -d $(bindir) || \
	    ($(mkdir) $(bindir) && $(chmod) $(dmode) $(bindir))
	@cd $(bindir) && $(rm) -f $(PROGS)
	@$(cp) -p $(PROGS) $(bindir)
	@cd $(bindir) && (test ! -f $(strip) || $(strip) $(PROGS))
	@cd $(bindir) && $(chmod) $(emode) $(PROGS)
	@test -d $(mandir) || \
	    ($(mkdir) $(mandir) && $(chmod) $(dmode) $(mandir))
	@echo "Installing formatted man pages: $(mandir)/cat1 ..."
	@test -d $(mandir)/cat1 || \
	    ($(mkdir) $(mandir)/cat1 && $(chmod) $(dmode) $(mandir)/cat1)
	@cd $(mandir)/cat1 && $(rm) -f $(man10)
	@cd $(srcdir)/man/man.text && $(cp) -p $(man10) $(mandir)/cat1
	@cd $(mandir)/cat1 && $(chmod) $(fmode) $(man10)
	@echo "Installing formatted man pages: $(mandir)/cat3 ..."
	@test -d $(mandir)/cat3 || \
	    ($(mkdir) $(mandir)/cat3 && $(chmod) $(dmode) $(mandir)/cat3)
	@cd $(mandir)/cat3 && $(rm) -f $(man30) $(manx30)
	@cd $(srcdir)/man/man.text && $(cp) -p $(man30) $(mandir)/cat3
	@cd $(srcdir)/man/mancxx.text && $(cp) -p $(manx30) $(mandir)/cat3
	@cd $(mandir)/cat3 && $(chmod) $(fmode) $(man30) $(manx30)
	@echo "Installing unformatted man pages: $(mandir)/man1 ..."
	@test -d $(mandir)/man1 || \
	    ($(mkdir) $(mandir)/man1 && $(chmod) $(dmode) $(mandir)/man1)
	@cd $(mandir)/man1 && $(rm) -f $(man1)
	@cd $(srcdir)/man/man.roff && $(cp) -p $(man1) $(mandir)/man1
	@cd $(mandir)/man1 && $(chmod) $(fmode) $(man1)
	@echo "Installing unformatted man pages: $(mandir)/man3 ..."
	@test -d $(mandir)/man3 || \
	    ($(mkdir) $(mandir)/man3 && $(chmod) $(dmode) $(mandir)/man3)
	@cd $(mandir)/man3 && $(rm) -f $(man3) $(manx3)
	@cd $(srcdir)/man/man.roff && $(cp) -p $(man3) $(mandir)/man3
	@cd $(srcdir)/man/mancxx.roff && $(cp) -p $(manx3) $(mandir)/man3
	@cd $(mandir)/man3 && $(chmod) $(fmode) $(man3) $(manx3)

uninstall:
	-cd $(includedir) && $(rm) -f db.h db_185.h db_cxx.h
	-cd $(libdir) && $(rm) -f $(libdb)
	-cd $(bindir) && $(rm) -f $(PROGS)
	-cd $(mandir)/man1 && $(rm) -f $(man1)
	-cd $(mandir)/cat1 && $(rm) -f $(man10)
	-cd $(mandir)/man3 && $(rm) -f $(man3) $(manx3)
	-cd $(mandir)/cat3 && $(rm) -f $(man30) $(manx30)

# Programs.
db_archive.obj: [-.db_archive]db_archive.c
	$(CC) $(CFLAGS) $?
db_checkpoint.obj: [-.db_checkpoint]db_checkpoint.c
	$(CC) $(CFLAGS) $?
db_deadlock.obj: [-.db_deadlock]db_deadlock.c
	$(CC) $(CFLAGS) $?
db_dump.obj: [-.db_dump]db_dump.c
	$(CC) $(CFLAGS) $?
db_dump185.obj: [-.db_dump185]db_dump185.c
	$(CC) $(CFLAGS) $?
db_load.obj: [-.db_load]db_load.c
	$(CC) $(CFLAGS) $?
db_printlog.obj: [-.db_printlog]db_printlog.c
	$(CC) $(CFLAGS) $?
db_recover.obj: [-.db_recover]db_recover.c
	$(CC) $(CFLAGS) $?
db_stat.obj: [-.db_stat]db_stat.c
	$(CC) $(CFLAGS) $?

# Examples.
ex_access.obj: [-.examples]ex_access.c
	$(CC) $(CFLAGS) $?
ex_appinit.obj: [-.examples]ex_appinit.c
	$(CC) $(CFLAGS) $?
ex_btrec.obj: [-.examples]ex_btrec.c
	$(CC) $(CFLAGS) $?
ex_lock.obj: [-.examples]ex_lock.c
	$(CC) $(CFLAGS) $?
ex_mpool.obj: [-.examples]ex_mpool.c
	$(CC) $(CFLAGS) $?
ex_thread.obj: [-.examples]ex_thread.c
	$(CC) $(CFLAGS) $?
ex_tpcb.obj: [-.examples]ex_tpcb.c
	$(CC) $(CFLAGS) $?

# DB files.
$(libdb)(db): [-.db]db.c
	$(CC) $(CFLAGS) $?
        libr/replace $(libdb) db.obj
        delete db.obj;*

$(libdb)(db_am): [-.db]db_am.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_am.obj
	delete db_am.obj;*

$(libdb)(db_auto): [-.db]db_auto.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_auto.obj
	delete db_auto.obj;*

$(libdb)(db_conv): [-.db]db_conv.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_conv.obj
	delete db_conv.obj;*

$(libdb)(db_dispatch): [-.db]db_dispatch.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_dispatch.obj
	delete db_dispatch.obj;*

$(libdb)(db_dup): [-.db]db_dup.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_dup.obj
	delete db_dup.obj;*

$(libdb)(db_iface): [-.db]db_iface.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_iface.obj
	delete db_iface.obj;*

$(libdb)(db_join): [-.db]db_join.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_join.obj
	delete db_join.obj;*

$(libdb)(db_overflow): [-.db]db_overflow.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_overflow.obj
	delete db_overflow.obj;*

$(libdb)(db_pr): [-.db]db_pr.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_pr.obj
	delete db_pr.obj;*

$(libdb)(db_rec): [-.db]db_rec.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_rec.obj
	delete db_rec.obj;*

$(libdb)(db_ret): [-.db]db_ret.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_ret.obj
	delete db_ret.obj;*

$(libdb)(db_thread): [-.db]db_thread.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_thread.obj
	delete db_thread.obj;*

# Btree source files.
$(libdb)(bt_close): [-.btree]bt_close.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_close.obj
	delete bt_close.obj;*

$(libdb)(bt_compare): [-.btree]bt_compare.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_compare.obj
	delete bt_compare.obj;*

$(libdb)(bt_conv): [-.btree]bt_conv.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_conv.obj
	delete bt_conv.obj;*

$(libdb)(bt_curadj): [-.btree]bt_curadj.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_curadj.obj
	delete bt_curadj.obj;*

$(libdb)(bt_cursor): [-.btree]bt_cursor.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_cursor.obj
	delete bt_cursor.obj;*

$(libdb)(bt_delete): [-.btree]bt_delete.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_delete.obj
	delete bt_delete.obj;*

$(libdb)(bt_open): [-.btree]bt_open.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_open.obj
	delete bt_open.obj;*

$(libdb)(bt_page): [-.btree]bt_page.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_page.obj
	delete bt_page.obj;*

$(libdb)(bt_put): [-.btree]bt_put.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_put.obj
	delete bt_put.obj;*

$(libdb)(bt_rec): [-.btree]bt_rec.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_rec.obj
	delete bt_rec.obj;*

$(libdb)(bt_recno): [-.btree]bt_recno.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_recno.obj
	delete bt_recno.obj;*

$(libdb)(bt_rsearch): [-.btree]bt_rsearch.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_rsearch.obj
	delete bt_rsearch.obj;*

$(libdb)(bt_search): [-.btree]bt_search.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_search.obj
	delete bt_search.obj;*

$(libdb)(bt_split): [-.btree]bt_split.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_split.obj
	delete bt_split.obj;*

$(libdb)(bt_stat): [-.btree]bt_stat.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) bt_stat.obj
	delete bt_stat.obj;*

$(libdb)(btree_auto): [-.btree]btree_auto.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) btree_auto.obj
	delete btree_auto.obj;*

# C++ interface files.
cxx_app.obj: [-.cxx]cxx_app.cpp
	$(CXX) $(CFLAGS) $?
cxx_except.obj: [-.cxx]cxx_except.cpp
	$(CXX) $(CFLAGS) $?
cxx_lock.obj: [-.cxx]cxx_lock.cpp
	$(CXX) $(CFLAGS) $?
cxx_log.obj: [-.cxx]cxx_log.cpp
	$(CXX) $(CFLAGS) $?
cxx_mpool.obj: [-.cxx]cxx_mpool.cpp
	$(CXX) $(CFLAGS) $?
cxx_table.obj: [-.cxx]cxx_table.cpp
	$(CXX) $(CFLAGS) $?
cxx_txn.obj: [-.cxx]cxx_txn.cpp
	$(CXX) $(CFLAGS) $?

# Hash source files.
$(libdb)(hash_auto): [-.hash]hash_auto.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_auto.obj
	delete hash_auto.obj;*

$(libdb)(hash): [-.hash]hash.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash.obj
	delete hash.obj;*

$(libdb)(hash_conv): [-.hash]hash_conv.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_conv.obj
	delete hash_conv.obj;*

$(libdb)(hash_debug): [-.hash]hash_debug.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_debug.obj
	delete hash_debug.obj;*

$(libdb)(hash_dup): [-.hash]hash_dup.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_dup.obj
	delete hash_dup.obj;*

$(libdb)(hash_func): [-.hash]hash_func.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_func.obj
	delete hash_func.obj;*

$(libdb)(hash_page): [-.hash]hash_page.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_page.obj
	delete hash_page.obj;*

$(libdb)(hash_rec): [-.hash]hash_rec.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_rec.obj
	delete hash_rec.obj;*

$(libdb)(hash_stat): [-.hash]hash_stat.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hash_stat.obj
	delete hash_stat.obj;*


# Lock source files.
$(libdb)(lock): [-.lock]lock.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) lock.obj
	delete lock.obj;*

$(libdb)(lock_conflict): [-.lock]lock_conflict.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) lock_conflict.obj
	delete lock_conflict.obj;*

$(libdb)(lock_deadlock): [-.lock]lock_deadlock.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) lock_deadlock.obj
	delete lock_deadlock.obj;*

$(libdb)(lock_region): [-.lock]lock_region.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) lock_region.obj
	delete lock_region.obj;*

$(libdb)(lock_util): [-.lock]lock_util.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) lock_util.obj
	delete lock_util.obj;*

# Log source files.
$(libdb)(log): [-.log]log.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log.obj
	delete log.obj;*

$(libdb)(log_archive): [-.log]log_archive.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_archive.obj
	delete log_archive.obj;*

$(libdb)(log_auto): [-.log]log_auto.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_auto.obj
	delete log_auto.obj;*

$(libdb)(log_compare): [-.log]log_compare.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_compare.obj
	delete log_compare.obj;*

$(libdb)(log_findckp): [-.log]log_findckp.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_findckp.obj
	delete log_findckp.obj;*

$(libdb)(log_get): [-.log]log_get.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_get.obj
	delete log_get.obj;*

$(libdb)(log_put): [-.log]log_put.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_put.obj
	delete log_put.obj;*

$(libdb)(log_rec): [-.log]log_rec.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_rec.obj
	delete log_rec.obj;*

$(libdb)(log_register): [-.log]log_register.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) log_register.obj
	delete log_register.obj;*

# Mpool source files.
$(libdb)(mp_bh): [-.mp]mp_bh.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_bh.obj
	delete mp_bh.obj;*

$(libdb)(mp_fget): [-.mp]mp_fget.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_fget.obj
	delete mp_fget.obj;*

$(libdb)(mp_fopen): [-.mp]mp_fopen.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_fopen.obj
	delete mp_fopen.obj;*

$(libdb)(mp_fput): [-.mp]mp_fput.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_fput.obj
	delete mp_fput.obj;*

$(libdb)(mp_fset): [-.mp]mp_fset.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_fset.obj
	delete mp_fset.obj;*

$(libdb)(mp_open): [-.mp]mp_open.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_open.obj
	delete mp_open.obj;*

$(libdb)(mp_pr): [-.mp]mp_pr.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_pr.obj
	delete mp_pr.obj;*

$(libdb)(mp_region): [-.mp]mp_region.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_region.obj
	delete mp_region.obj;*

$(libdb)(mp_sync): [-.mp]mp_sync.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mp_sync.obj
	delete mp_sync.obj;*

# Mutex source files.
$(libdb)(mutex): [-.mutex]mutex.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) mutex.obj
	delete mutex.obj;*

# XXX
# UTS4 spinlocks
uts4.cc.obj: [-.mutex]uts4.cc.s
	$(AS) $(ASFLAGS) -o $@ $?

# Transaction source files.
$(libdb)(txn): [-.txn]txn.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) txn.obj
	delete txn.obj;*

$(libdb)(txn_auto): [-.txn]txn_auto.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) txn_auto.obj
	delete txn_auto.obj;*

$(libdb)(txn_rec): [-.txn]txn_rec.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) txn_rec.obj
	delete txn_rec.obj;*

# Historic interfaces.
$(libdb)(hsearch): [-.hsearch]hsearch.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) hsearch.obj
	delete hsearch.obj;*

$(libdb)(dbm): [-.dbm]dbm.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) dbm.obj
	delete dbm.obj;*

# Common source files.
$(libdb)(db_appinit): [-.common]db_appinit.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_appinit.obj
	delete db_appinit.obj;*

$(libdb)(db_apprec): [-.common]db_apprec.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_apprec.obj
	delete db_apprec.obj;*

$(libdb)(db_byteorder): [-.common]db_byteorder.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_byteorder.obj
	delete db_byteorder.obj;*

$(libdb)(db_err): [-.common]db_err.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_err.obj
	delete db_err.obj;*

$(libdb)(db_log2): [-.common]db_log2.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_log2.obj
	delete db_log2.obj;*

$(libdb)(db_region): [-.common]db_region.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_region.obj
	delete db_region.obj;*

$(libdb)(db_salloc): [-.common]db_salloc.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_salloc.obj
	delete db_salloc.obj;*

$(libdb)(db_shash): [-.common]db_shash.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) db_shash.obj
	delete db_shash.obj;*

# OS specific source files.
$(libdb)(os_abs): [-.os]os_abs.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_abs.obj
	delete os_abs.obj;*

$(libdb)(os_alloc): [-.os]os_alloc.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_alloc.obj
	delete os_alloc.obj;*

$(libdb)(os_config): [-.os]os_config.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_config.obj
	delete os_config.obj;*

$(libdb)(os_dir): [-.os]os_dir.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_dir.obj
	delete os_dir.obj;*

$(libdb)(os_fid): [-.os]os_fid.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_fid.obj
	delete os_fid.obj;*

$(libdb)(os_fsync): [-.os]os_fsync.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_fsync.obj
	delete os_fsync.obj;*

$(libdb)(os_map): [-.os]os_map.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_map.obj
	delete os_map.obj;*

$(libdb)(os_oflags): [-.os]os_oflags.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_oflags.obj
	delete os_oflags.obj;*

$(libdb)(os_open): [-.os]os_open.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_open.obj
	delete os_open.obj;*

$(libdb)(os_rpath): [-.os]os_rpath.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_rpath.obj
	delete os_rpath.obj;*

$(libdb)(os_rw): [-.os]os_rw.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_rw.obj
	delete os_rw.obj;*

$(libdb)(os_seek): [-.os]os_seek.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_seek.obj
	delete os_seek.obj;*

$(libdb)(os_sleep): [-.os]os_sleep.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_sleep.obj
	delete os_sleep.obj;*

$(libdb)(os_spin): [-.os]os_spin.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_spin.obj
	delete os_spin.obj;*

$(libdb)(os_stat): [-.os]os_stat.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_stat.obj
	delete os_stat.obj;*

$(libdb)(os_tmpdir): [-.os]os_tmpdir.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_tmpdir.obj
	delete os_tmpdir.obj;*

$(libdb)(os_unlink): [-.os]os_unlink.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) os_unlink.obj
	delete os_unlink.obj;*

# Test programs.
.dbtestrc: db.h [-.test]test.tcl
	copy/log [-.test]test.tcl $@
tclAppInit.obj: [-.test]tclAppInit.c
	$(CC) $(CFLAGS) $?
tcl_dbm.obj: [-.test]tcl_dbm.c
	$(CC) $(CFLAGS) $?
tcl_hsearch.obj: [-.test]tcl_hsearch.c
	$(CC) $(CFLAGS) $?
tcl_lock.obj: [-.test]tcl_lock.c
	$(CC) $(CFLAGS) $?
tcl_log.obj: [-.test]tcl_log.c
	$(CC) $(CFLAGS) $?
tcl_mpool.obj: [-.test]tcl_mpool.c
	$(CC) $(CFLAGS) $?
tcl_mutex.obj: [-.test]tcl_mutex.c
	$(CC) $(CFLAGS) $?
tcl_ndbm.obj: [-.test]tcl_ndbm.c
	$(CC) $(CFLAGS) $?
tcl_txn.obj: [-.test]tcl_txn.c
	$(CC) $(CFLAGS) $?
thread.obj: [-.test]thread.c
	$(CC) $(CFLAGS) $?
utils.obj: [-.test]utils.c
	$(CC) $(CFLAGS) $?

# Replacement source files.
err.obj: [-.clib]err.c
	$(CC) $(CFLAGS) $?
getcwd.obj: [-.clib]getcwd.c
	$(CC) $(CFLAGS) $?
getlong.obj: [-.clib]getlong.c
	$(CC) $(CFLAGS) $?
getopt.obj: [-.clib]getopt.c
	$(CC) $(CFLAGS) $?
memcmp.obj: [-.clib]memcmp.c
	$(CC) $(CFLAGS) $?
memcpy.obj: [-.clib]memcpy.c
	$(CC) -DMEMCOPY $(CFLAGS) $?
memmove.obj: [-.clib]memmove.c
	$(CC) -DMEMMOVE $(CFLAGS) $?
raise.obj: [-.clib]raise.c
	$(CC) $(CFLAGS) $?

$(libdb)(snprintf): [-.clib]snprintf.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) snprintf.obj
	delete snprintf.obj;*

strdup.obj: [-.clib]strdup.c
	$(CC) $(CFLAGS) $?
strerror.obj: [-.clib]strerror.c
	$(CC) $(CFLAGS) $?
strsep.obj: [-.clib]strsep.c
	$(CC) $(CFLAGS) $?

$(libdb)(vsnprintf): [-.clib]vsnprintf.c
	$(CC) $(CFLAGS) $?
	libr/replace $(libdb) vsnprintf.obj
	delete vsnprintf.obj;*
