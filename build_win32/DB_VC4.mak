# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=DB_DLL - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to DB_DLL - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "DB_DLL - Win32 Release" && "$(CFG)" != "DB_DLL - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "DB_VC4.mak" CFG="DB_DLL - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "DB_DLL - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "DB_DLL - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "DB_DLL - Win32 Debug"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

!IF  "$(CFG)" == "DB_DLL - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "DB_DLL__"
# PROP BASE Intermediate_Dir "DB_DLL__"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release_VC4"
# PROP Intermediate_Dir "Release_VC4"
# PROP Target_Dir ""
OUTDIR=.\Release_VC4
INTDIR=.\Release_VC4

ALL : "$(OUTDIR)\libdb30.dll"

CLEAN : 
	-@erase "$(INTDIR)\bt_compare.obj"
	-@erase "$(INTDIR)\bt_conv.obj"
	-@erase "$(INTDIR)\bt_curadj.obj"
	-@erase "$(INTDIR)\bt_cursor.obj"
	-@erase "$(INTDIR)\bt_delete.obj"
	-@erase "$(INTDIR)\bt_method.obj"
	-@erase "$(INTDIR)\bt_open.obj"
	-@erase "$(INTDIR)\bt_put.obj"
	-@erase "$(INTDIR)\bt_rec.obj"
	-@erase "$(INTDIR)\bt_reclaim.obj"
	-@erase "$(INTDIR)\bt_recno.obj"
	-@erase "$(INTDIR)\bt_rsearch.obj"
	-@erase "$(INTDIR)\bt_search.obj"
	-@erase "$(INTDIR)\bt_split.obj"
	-@erase "$(INTDIR)\bt_stat.obj"
	-@erase "$(INTDIR)\bt_upgrade.obj"
	-@erase "$(INTDIR)\btree_auto.obj"
	-@erase "$(INTDIR)\crdel_auto.obj"
	-@erase "$(INTDIR)\crdel_rec.obj"
	-@erase "$(INTDIR)\cxx_app.obj"
	-@erase "$(INTDIR)\cxx_except.obj"
	-@erase "$(INTDIR)\cxx_lock.obj"
	-@erase "$(INTDIR)\cxx_log.obj"
	-@erase "$(INTDIR)\cxx_mpool.obj"
	-@erase "$(INTDIR)\cxx_table.obj"
	-@erase "$(INTDIR)\cxx_txn.obj"
	-@erase "$(INTDIR)\db.obj"
	-@erase "$(INTDIR)\db_am.obj"
	-@erase "$(INTDIR)\db_auto.obj"
	-@erase "$(INTDIR)\db_byteorder.obj"
	-@erase "$(INTDIR)\db_conv.obj"
	-@erase "$(INTDIR)\db_dispatch.obj"
	-@erase "$(INTDIR)\db_dup.obj"
	-@erase "$(INTDIR)\db_err.obj"
	-@erase "$(INTDIR)\db_getlong.obj"
	-@erase "$(INTDIR)\db_iface.obj"
	-@erase "$(INTDIR)\db_join.obj"
	-@erase "$(INTDIR)\db_log2.obj"
	-@erase "$(INTDIR)\db_meta.obj"
	-@erase "$(INTDIR)\db_method.obj"
	-@erase "$(INTDIR)\db_overflow.obj"
	-@erase "$(INTDIR)\db_pr.obj"
	-@erase "$(INTDIR)\db_rec.obj"
	-@erase "$(INTDIR)\db_reclaim.obj"
	-@erase "$(INTDIR)\db_ret.obj"
	-@erase "$(INTDIR)\db_salloc.obj"
	-@erase "$(INTDIR)\db_shash.obj"
	-@erase "$(INTDIR)\db_upgrade.obj"
	-@erase "$(INTDIR)\dbm.obj"
	-@erase "$(INTDIR)\dllmain.obj"
	-@erase "$(INTDIR)\env_method.obj"
	-@erase "$(INTDIR)\env_open.obj"
	-@erase "$(INTDIR)\env_recover.obj"
	-@erase "$(INTDIR)\env_region.obj"
	-@erase "$(INTDIR)\hash.obj"
	-@erase "$(INTDIR)\hash_auto.obj"
	-@erase "$(INTDIR)\hash_conv.obj"
	-@erase "$(INTDIR)\hash_dup.obj"
	-@erase "$(INTDIR)\hash_func.obj"
	-@erase "$(INTDIR)\hash_meta.obj"
	-@erase "$(INTDIR)\hash_method.obj"
	-@erase "$(INTDIR)\hash_page.obj"
	-@erase "$(INTDIR)\hash_rec.obj"
	-@erase "$(INTDIR)\hash_reclaim.obj"
	-@erase "$(INTDIR)\hash_stat.obj"
	-@erase "$(INTDIR)\hash_upgrade.obj"
	-@erase "$(INTDIR)\hsearch.obj"
	-@erase "$(INTDIR)\libdb.res"
	-@erase "$(INTDIR)\lock.obj"
	-@erase "$(INTDIR)\lock_conflict.obj"
	-@erase "$(INTDIR)\lock_deadlock.obj"
	-@erase "$(INTDIR)\lock_region.obj"
	-@erase "$(INTDIR)\lock_util.obj"
	-@erase "$(INTDIR)\log.obj"
	-@erase "$(INTDIR)\log_archive.obj"
	-@erase "$(INTDIR)\log_auto.obj"
	-@erase "$(INTDIR)\log_compare.obj"
	-@erase "$(INTDIR)\log_findckp.obj"
	-@erase "$(INTDIR)\log_get.obj"
	-@erase "$(INTDIR)\log_method.obj"
	-@erase "$(INTDIR)\log_put.obj"
	-@erase "$(INTDIR)\log_rec.obj"
	-@erase "$(INTDIR)\log_register.obj"
	-@erase "$(INTDIR)\mp_alloc.obj"
	-@erase "$(INTDIR)\mp_bh.obj"
	-@erase "$(INTDIR)\mp_fget.obj"
	-@erase "$(INTDIR)\mp_fopen.obj"
	-@erase "$(INTDIR)\mp_fput.obj"
	-@erase "$(INTDIR)\mp_fset.obj"
	-@erase "$(INTDIR)\mp_method.obj"
	-@erase "$(INTDIR)\mp_region.obj"
	-@erase "$(INTDIR)\mp_register.obj"
	-@erase "$(INTDIR)\mp_stat.obj"
	-@erase "$(INTDIR)\mp_sync.obj"
	-@erase "$(INTDIR)\mp_trickle.obj"
	-@erase "$(INTDIR)\mut_tas.obj"
	-@erase "$(INTDIR)\mutex.obj"
	-@erase "$(INTDIR)\os_abs.obj"
	-@erase "$(INTDIR)\os_alloc.obj"
	-@erase "$(INTDIR)\os_dir.obj"
	-@erase "$(INTDIR)\os_errno.obj"
	-@erase "$(INTDIR)\os_fid.obj"
	-@erase "$(INTDIR)\os_finit.obj"
	-@erase "$(INTDIR)\os_fsync.obj"
	-@erase "$(INTDIR)\os_handle.obj"
	-@erase "$(INTDIR)\os_map.obj"
	-@erase "$(INTDIR)\os_method.obj"
	-@erase "$(INTDIR)\os_oflags.obj"
	-@erase "$(INTDIR)\os_open.obj"
	-@erase "$(INTDIR)\os_region.obj"
	-@erase "$(INTDIR)\os_rename.obj"
	-@erase "$(INTDIR)\os_root.obj"
	-@erase "$(INTDIR)\os_rpath.obj"
	-@erase "$(INTDIR)\os_rw.obj"
	-@erase "$(INTDIR)\os_seek.obj"
	-@erase "$(INTDIR)\os_sleep.obj"
	-@erase "$(INTDIR)\os_spin.obj"
	-@erase "$(INTDIR)\os_stat.obj"
	-@erase "$(INTDIR)\os_tmpdir.obj"
	-@erase "$(INTDIR)\os_type.obj"
	-@erase "$(INTDIR)\os_unlink.obj"
	-@erase "$(INTDIR)\qam.obj"
	-@erase "$(INTDIR)\qam_auto.obj"
	-@erase "$(INTDIR)\qam_conv.obj"
	-@erase "$(INTDIR)\qam_method.obj"
	-@erase "$(INTDIR)\qam_open.obj"
	-@erase "$(INTDIR)\qam_rec.obj"
	-@erase "$(INTDIR)\qam_stat.obj"
	-@erase "$(INTDIR)\txn.obj"
	-@erase "$(INTDIR)\txn_auto.obj"
	-@erase "$(INTDIR)\txn_rec.obj"
	-@erase "$(INTDIR)\txn_region.obj"
	-@erase "$(INTDIR)\xa.obj"
	-@erase "$(INTDIR)\xa_db.obj"
	-@erase "$(INTDIR)\xa_map.obj"
	-@erase "$(OUTDIR)\libdb30.dll"
	-@erase "$(OUTDIR)\libdb30.exp"
	-@erase "$(OUTDIR)\libdb30.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /Ob2 /I "." /I "../include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "DB_CREATE_DLL" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /O2 /Ob2 /I "." /I "../include" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "DB_CREATE_DLL" /Fp"$(INTDIR)/DB_VC4.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release_VC4/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/libdb.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/DB_VC4.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 /nologo /base:0x13000000 /subsystem:windows /dll /machine:I386 /out:"Release_VC4/libdb30.dll"
LINK32_FLAGS=/nologo /base:0x13000000 /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/libdb30.pdb" /machine:I386 /def:".\libdb.def"\
 /out:"$(OUTDIR)/libdb30.dll" /implib:"$(OUTDIR)/libdb30.lib" 
DEF_FILE= \
	".\libdb.def"
LINK32_OBJS= \
	"$(INTDIR)\bt_compare.obj" \
	"$(INTDIR)\bt_conv.obj" \
	"$(INTDIR)\bt_curadj.obj" \
	"$(INTDIR)\bt_cursor.obj" \
	"$(INTDIR)\bt_delete.obj" \
	"$(INTDIR)\bt_method.obj" \
	"$(INTDIR)\bt_open.obj" \
	"$(INTDIR)\bt_put.obj" \
	"$(INTDIR)\bt_rec.obj" \
	"$(INTDIR)\bt_reclaim.obj" \
	"$(INTDIR)\bt_recno.obj" \
	"$(INTDIR)\bt_rsearch.obj" \
	"$(INTDIR)\bt_search.obj" \
	"$(INTDIR)\bt_split.obj" \
	"$(INTDIR)\bt_stat.obj" \
	"$(INTDIR)\bt_upgrade.obj" \
	"$(INTDIR)\btree_auto.obj" \
	"$(INTDIR)\crdel_auto.obj" \
	"$(INTDIR)\crdel_rec.obj" \
	"$(INTDIR)\cxx_app.obj" \
	"$(INTDIR)\cxx_except.obj" \
	"$(INTDIR)\cxx_lock.obj" \
	"$(INTDIR)\cxx_log.obj" \
	"$(INTDIR)\cxx_mpool.obj" \
	"$(INTDIR)\cxx_table.obj" \
	"$(INTDIR)\cxx_txn.obj" \
	"$(INTDIR)\db.obj" \
	"$(INTDIR)\db_am.obj" \
	"$(INTDIR)\db_auto.obj" \
	"$(INTDIR)\db_byteorder.obj" \
	"$(INTDIR)\db_conv.obj" \
	"$(INTDIR)\db_dispatch.obj" \
	"$(INTDIR)\db_dup.obj" \
	"$(INTDIR)\db_err.obj" \
	"$(INTDIR)\db_getlong.obj" \
	"$(INTDIR)\db_iface.obj" \
	"$(INTDIR)\db_join.obj" \
	"$(INTDIR)\db_log2.obj" \
	"$(INTDIR)\db_meta.obj" \
	"$(INTDIR)\db_method.obj" \
	"$(INTDIR)\db_overflow.obj" \
	"$(INTDIR)\db_pr.obj" \
	"$(INTDIR)\db_rec.obj" \
	"$(INTDIR)\db_reclaim.obj" \
	"$(INTDIR)\db_ret.obj" \
	"$(INTDIR)\db_salloc.obj" \
	"$(INTDIR)\db_shash.obj" \
	"$(INTDIR)\db_upgrade.obj" \
	"$(INTDIR)\dbm.obj" \
	"$(INTDIR)\dllmain.obj" \
	"$(INTDIR)\env_method.obj" \
	"$(INTDIR)\env_open.obj" \
	"$(INTDIR)\env_recover.obj" \
	"$(INTDIR)\env_region.obj" \
	"$(INTDIR)\hash.obj" \
	"$(INTDIR)\hash_auto.obj" \
	"$(INTDIR)\hash_conv.obj" \
	"$(INTDIR)\hash_dup.obj" \
	"$(INTDIR)\hash_func.obj" \
	"$(INTDIR)\hash_meta.obj" \
	"$(INTDIR)\hash_method.obj" \
	"$(INTDIR)\hash_page.obj" \
	"$(INTDIR)\hash_rec.obj" \
	"$(INTDIR)\hash_reclaim.obj" \
	"$(INTDIR)\hash_stat.obj" \
	"$(INTDIR)\hash_upgrade.obj" \
	"$(INTDIR)\hsearch.obj" \
	"$(INTDIR)\libdb.res" \
	"$(INTDIR)\lock.obj" \
	"$(INTDIR)\lock_conflict.obj" \
	"$(INTDIR)\lock_deadlock.obj" \
	"$(INTDIR)\lock_region.obj" \
	"$(INTDIR)\lock_util.obj" \
	"$(INTDIR)\log.obj" \
	"$(INTDIR)\log_archive.obj" \
	"$(INTDIR)\log_auto.obj" \
	"$(INTDIR)\log_compare.obj" \
	"$(INTDIR)\log_findckp.obj" \
	"$(INTDIR)\log_get.obj" \
	"$(INTDIR)\log_method.obj" \
	"$(INTDIR)\log_put.obj" \
	"$(INTDIR)\log_rec.obj" \
	"$(INTDIR)\log_register.obj" \
	"$(INTDIR)\mp_alloc.obj" \
	"$(INTDIR)\mp_bh.obj" \
	"$(INTDIR)\mp_fget.obj" \
	"$(INTDIR)\mp_fopen.obj" \
	"$(INTDIR)\mp_fput.obj" \
	"$(INTDIR)\mp_fset.obj" \
	"$(INTDIR)\mp_method.obj" \
	"$(INTDIR)\mp_region.obj" \
	"$(INTDIR)\mp_register.obj" \
	"$(INTDIR)\mp_stat.obj" \
	"$(INTDIR)\mp_sync.obj" \
	"$(INTDIR)\mp_trickle.obj" \
	"$(INTDIR)\mut_tas.obj" \
	"$(INTDIR)\mutex.obj" \
	"$(INTDIR)\os_abs.obj" \
	"$(INTDIR)\os_alloc.obj" \
	"$(INTDIR)\os_dir.obj" \
	"$(INTDIR)\os_errno.obj" \
	"$(INTDIR)\os_fid.obj" \
	"$(INTDIR)\os_finit.obj" \
	"$(INTDIR)\os_fsync.obj" \
	"$(INTDIR)\os_handle.obj" \
	"$(INTDIR)\os_map.obj" \
	"$(INTDIR)\os_method.obj" \
	"$(INTDIR)\os_oflags.obj" \
	"$(INTDIR)\os_open.obj" \
	"$(INTDIR)\os_region.obj" \
	"$(INTDIR)\os_rename.obj" \
	"$(INTDIR)\os_root.obj" \
	"$(INTDIR)\os_rpath.obj" \
	"$(INTDIR)\os_rw.obj" \
	"$(INTDIR)\os_seek.obj" \
	"$(INTDIR)\os_sleep.obj" \
	"$(INTDIR)\os_spin.obj" \
	"$(INTDIR)\os_stat.obj" \
	"$(INTDIR)\os_tmpdir.obj" \
	"$(INTDIR)\os_type.obj" \
	"$(INTDIR)\os_unlink.obj" \
	"$(INTDIR)\qam.obj" \
	"$(INTDIR)\qam_auto.obj" \
	"$(INTDIR)\qam_conv.obj" \
	"$(INTDIR)\qam_method.obj" \
	"$(INTDIR)\qam_open.obj" \
	"$(INTDIR)\qam_rec.obj" \
	"$(INTDIR)\qam_stat.obj" \
	"$(INTDIR)\txn.obj" \
	"$(INTDIR)\txn_auto.obj" \
	"$(INTDIR)\txn_rec.obj" \
	"$(INTDIR)\txn_region.obj" \
	"$(INTDIR)\xa.obj" \
	"$(INTDIR)\xa_db.obj" \
	"$(INTDIR)\xa_map.obj"

"$(OUTDIR)\libdb30.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DB_DLL - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "DB_DLL_0"
# PROP BASE Intermediate_Dir "DB_DLL_0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug_VC4"
# PROP Intermediate_Dir "Debug_VC4"
# PROP Target_Dir ""
OUTDIR=.\Debug_VC4
INTDIR=.\Debug_VC4

ALL : "$(OUTDIR)\libdb30d.dll"

CLEAN : 
	-@erase "$(INTDIR)\bt_compare.obj"
	-@erase "$(INTDIR)\bt_conv.obj"
	-@erase "$(INTDIR)\bt_curadj.obj"
	-@erase "$(INTDIR)\bt_cursor.obj"
	-@erase "$(INTDIR)\bt_delete.obj"
	-@erase "$(INTDIR)\bt_method.obj"
	-@erase "$(INTDIR)\bt_open.obj"
	-@erase "$(INTDIR)\bt_put.obj"
	-@erase "$(INTDIR)\bt_rec.obj"
	-@erase "$(INTDIR)\bt_reclaim.obj"
	-@erase "$(INTDIR)\bt_recno.obj"
	-@erase "$(INTDIR)\bt_rsearch.obj"
	-@erase "$(INTDIR)\bt_search.obj"
	-@erase "$(INTDIR)\bt_split.obj"
	-@erase "$(INTDIR)\bt_stat.obj"
	-@erase "$(INTDIR)\bt_upgrade.obj"
	-@erase "$(INTDIR)\btree_auto.obj"
	-@erase "$(INTDIR)\crdel_auto.obj"
	-@erase "$(INTDIR)\crdel_rec.obj"
	-@erase "$(INTDIR)\cxx_app.obj"
	-@erase "$(INTDIR)\cxx_except.obj"
	-@erase "$(INTDIR)\cxx_lock.obj"
	-@erase "$(INTDIR)\cxx_log.obj"
	-@erase "$(INTDIR)\cxx_mpool.obj"
	-@erase "$(INTDIR)\cxx_table.obj"
	-@erase "$(INTDIR)\cxx_txn.obj"
	-@erase "$(INTDIR)\db.obj"
	-@erase "$(INTDIR)\db_am.obj"
	-@erase "$(INTDIR)\db_auto.obj"
	-@erase "$(INTDIR)\db_byteorder.obj"
	-@erase "$(INTDIR)\db_conv.obj"
	-@erase "$(INTDIR)\db_dispatch.obj"
	-@erase "$(INTDIR)\db_dup.obj"
	-@erase "$(INTDIR)\db_err.obj"
	-@erase "$(INTDIR)\db_getlong.obj"
	-@erase "$(INTDIR)\db_iface.obj"
	-@erase "$(INTDIR)\db_join.obj"
	-@erase "$(INTDIR)\db_log2.obj"
	-@erase "$(INTDIR)\db_meta.obj"
	-@erase "$(INTDIR)\db_method.obj"
	-@erase "$(INTDIR)\db_overflow.obj"
	-@erase "$(INTDIR)\db_pr.obj"
	-@erase "$(INTDIR)\db_rec.obj"
	-@erase "$(INTDIR)\db_reclaim.obj"
	-@erase "$(INTDIR)\db_ret.obj"
	-@erase "$(INTDIR)\db_salloc.obj"
	-@erase "$(INTDIR)\db_shash.obj"
	-@erase "$(INTDIR)\db_upgrade.obj"
	-@erase "$(INTDIR)\dbm.obj"
	-@erase "$(INTDIR)\dllmain.obj"
	-@erase "$(INTDIR)\env_method.obj"
	-@erase "$(INTDIR)\env_open.obj"
	-@erase "$(INTDIR)\env_recover.obj"
	-@erase "$(INTDIR)\env_region.obj"
	-@erase "$(INTDIR)\hash.obj"
	-@erase "$(INTDIR)\hash_auto.obj"
	-@erase "$(INTDIR)\hash_conv.obj"
	-@erase "$(INTDIR)\hash_dup.obj"
	-@erase "$(INTDIR)\hash_func.obj"
	-@erase "$(INTDIR)\hash_meta.obj"
	-@erase "$(INTDIR)\hash_method.obj"
	-@erase "$(INTDIR)\hash_page.obj"
	-@erase "$(INTDIR)\hash_rec.obj"
	-@erase "$(INTDIR)\hash_reclaim.obj"
	-@erase "$(INTDIR)\hash_stat.obj"
	-@erase "$(INTDIR)\hash_upgrade.obj"
	-@erase "$(INTDIR)\hsearch.obj"
	-@erase "$(INTDIR)\libdb.res"
	-@erase "$(INTDIR)\lock.obj"
	-@erase "$(INTDIR)\lock_conflict.obj"
	-@erase "$(INTDIR)\lock_deadlock.obj"
	-@erase "$(INTDIR)\lock_region.obj"
	-@erase "$(INTDIR)\lock_util.obj"
	-@erase "$(INTDIR)\log.obj"
	-@erase "$(INTDIR)\log_archive.obj"
	-@erase "$(INTDIR)\log_auto.obj"
	-@erase "$(INTDIR)\log_compare.obj"
	-@erase "$(INTDIR)\log_findckp.obj"
	-@erase "$(INTDIR)\log_get.obj"
	-@erase "$(INTDIR)\log_method.obj"
	-@erase "$(INTDIR)\log_put.obj"
	-@erase "$(INTDIR)\log_rec.obj"
	-@erase "$(INTDIR)\log_register.obj"
	-@erase "$(INTDIR)\mp_alloc.obj"
	-@erase "$(INTDIR)\mp_bh.obj"
	-@erase "$(INTDIR)\mp_fget.obj"
	-@erase "$(INTDIR)\mp_fopen.obj"
	-@erase "$(INTDIR)\mp_fput.obj"
	-@erase "$(INTDIR)\mp_fset.obj"
	-@erase "$(INTDIR)\mp_method.obj"
	-@erase "$(INTDIR)\mp_region.obj"
	-@erase "$(INTDIR)\mp_register.obj"
	-@erase "$(INTDIR)\mp_stat.obj"
	-@erase "$(INTDIR)\mp_sync.obj"
	-@erase "$(INTDIR)\mp_trickle.obj"
	-@erase "$(INTDIR)\mut_tas.obj"
	-@erase "$(INTDIR)\mutex.obj"
	-@erase "$(INTDIR)\os_abs.obj"
	-@erase "$(INTDIR)\os_alloc.obj"
	-@erase "$(INTDIR)\os_dir.obj"
	-@erase "$(INTDIR)\os_errno.obj"
	-@erase "$(INTDIR)\os_fid.obj"
	-@erase "$(INTDIR)\os_finit.obj"
	-@erase "$(INTDIR)\os_fsync.obj"
	-@erase "$(INTDIR)\os_handle.obj"
	-@erase "$(INTDIR)\os_map.obj"
	-@erase "$(INTDIR)\os_method.obj"
	-@erase "$(INTDIR)\os_oflags.obj"
	-@erase "$(INTDIR)\os_open.obj"
	-@erase "$(INTDIR)\os_region.obj"
	-@erase "$(INTDIR)\os_rename.obj"
	-@erase "$(INTDIR)\os_root.obj"
	-@erase "$(INTDIR)\os_rpath.obj"
	-@erase "$(INTDIR)\os_rw.obj"
	-@erase "$(INTDIR)\os_seek.obj"
	-@erase "$(INTDIR)\os_sleep.obj"
	-@erase "$(INTDIR)\os_spin.obj"
	-@erase "$(INTDIR)\os_stat.obj"
	-@erase "$(INTDIR)\os_tmpdir.obj"
	-@erase "$(INTDIR)\os_type.obj"
	-@erase "$(INTDIR)\os_unlink.obj"
	-@erase "$(INTDIR)\qam.obj"
	-@erase "$(INTDIR)\qam_auto.obj"
	-@erase "$(INTDIR)\qam_conv.obj"
	-@erase "$(INTDIR)\qam_method.obj"
	-@erase "$(INTDIR)\qam_open.obj"
	-@erase "$(INTDIR)\qam_rec.obj"
	-@erase "$(INTDIR)\qam_stat.obj"
	-@erase "$(INTDIR)\txn.obj"
	-@erase "$(INTDIR)\txn_auto.obj"
	-@erase "$(INTDIR)\txn_rec.obj"
	-@erase "$(INTDIR)\txn_region.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\xa.obj"
	-@erase "$(INTDIR)\xa_db.obj"
	-@erase "$(INTDIR)\xa_map.obj"
	-@erase "$(OUTDIR)\libdb30d.dll"
	-@erase "$(OUTDIR)\libdb30d.exp"
	-@erase "$(OUTDIR)\libdb30d.ilk"
	-@erase "$(OUTDIR)\libdb30d.lib"
	-@erase "$(OUTDIR)\libdb30d.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /I "." /I "../include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "DB_CREATE_DLL" /YX /c
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /Zi /Od /I "." /I "../include" /D "_DEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "DB_CREATE_DLL" /Fp"$(INTDIR)/DB_VC4.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug_VC4/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/libdb.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/DB_VC4.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 /nologo /base:0x13000000 /subsystem:windows /dll /debug /machine:I386 /out:"Debug_VC4/libdb30d.dll"
LINK32_FLAGS=/nologo /base:0x13000000 /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)/libdb30d.pdb" /debug /machine:I386 /def:".\libdb.def"\
 /out:"$(OUTDIR)/libdb30d.dll" /implib:"$(OUTDIR)/libdb30d.lib" 
DEF_FILE= \
	".\libdb.def"
LINK32_OBJS= \
	"$(INTDIR)\bt_compare.obj" \
	"$(INTDIR)\bt_conv.obj" \
	"$(INTDIR)\bt_curadj.obj" \
	"$(INTDIR)\bt_cursor.obj" \
	"$(INTDIR)\bt_delete.obj" \
	"$(INTDIR)\bt_method.obj" \
	"$(INTDIR)\bt_open.obj" \
	"$(INTDIR)\bt_put.obj" \
	"$(INTDIR)\bt_rec.obj" \
	"$(INTDIR)\bt_reclaim.obj" \
	"$(INTDIR)\bt_recno.obj" \
	"$(INTDIR)\bt_rsearch.obj" \
	"$(INTDIR)\bt_search.obj" \
	"$(INTDIR)\bt_split.obj" \
	"$(INTDIR)\bt_stat.obj" \
	"$(INTDIR)\bt_upgrade.obj" \
	"$(INTDIR)\btree_auto.obj" \
	"$(INTDIR)\crdel_auto.obj" \
	"$(INTDIR)\crdel_rec.obj" \
	"$(INTDIR)\cxx_app.obj" \
	"$(INTDIR)\cxx_except.obj" \
	"$(INTDIR)\cxx_lock.obj" \
	"$(INTDIR)\cxx_log.obj" \
	"$(INTDIR)\cxx_mpool.obj" \
	"$(INTDIR)\cxx_table.obj" \
	"$(INTDIR)\cxx_txn.obj" \
	"$(INTDIR)\db.obj" \
	"$(INTDIR)\db_am.obj" \
	"$(INTDIR)\db_auto.obj" \
	"$(INTDIR)\db_byteorder.obj" \
	"$(INTDIR)\db_conv.obj" \
	"$(INTDIR)\db_dispatch.obj" \
	"$(INTDIR)\db_dup.obj" \
	"$(INTDIR)\db_err.obj" \
	"$(INTDIR)\db_getlong.obj" \
	"$(INTDIR)\db_iface.obj" \
	"$(INTDIR)\db_join.obj" \
	"$(INTDIR)\db_log2.obj" \
	"$(INTDIR)\db_meta.obj" \
	"$(INTDIR)\db_method.obj" \
	"$(INTDIR)\db_overflow.obj" \
	"$(INTDIR)\db_pr.obj" \
	"$(INTDIR)\db_rec.obj" \
	"$(INTDIR)\db_reclaim.obj" \
	"$(INTDIR)\db_ret.obj" \
	"$(INTDIR)\db_salloc.obj" \
	"$(INTDIR)\db_shash.obj" \
	"$(INTDIR)\db_upgrade.obj" \
	"$(INTDIR)\dbm.obj" \
	"$(INTDIR)\dllmain.obj" \
	"$(INTDIR)\env_method.obj" \
	"$(INTDIR)\env_open.obj" \
	"$(INTDIR)\env_recover.obj" \
	"$(INTDIR)\env_region.obj" \
	"$(INTDIR)\hash.obj" \
	"$(INTDIR)\hash_auto.obj" \
	"$(INTDIR)\hash_conv.obj" \
	"$(INTDIR)\hash_dup.obj" \
	"$(INTDIR)\hash_func.obj" \
	"$(INTDIR)\hash_meta.obj" \
	"$(INTDIR)\hash_method.obj" \
	"$(INTDIR)\hash_page.obj" \
	"$(INTDIR)\hash_rec.obj" \
	"$(INTDIR)\hash_reclaim.obj" \
	"$(INTDIR)\hash_stat.obj" \
	"$(INTDIR)\hash_upgrade.obj" \
	"$(INTDIR)\hsearch.obj" \
	"$(INTDIR)\libdb.res" \
	"$(INTDIR)\lock.obj" \
	"$(INTDIR)\lock_conflict.obj" \
	"$(INTDIR)\lock_deadlock.obj" \
	"$(INTDIR)\lock_region.obj" \
	"$(INTDIR)\lock_util.obj" \
	"$(INTDIR)\log.obj" \
	"$(INTDIR)\log_archive.obj" \
	"$(INTDIR)\log_auto.obj" \
	"$(INTDIR)\log_compare.obj" \
	"$(INTDIR)\log_findckp.obj" \
	"$(INTDIR)\log_get.obj" \
	"$(INTDIR)\log_method.obj" \
	"$(INTDIR)\log_put.obj" \
	"$(INTDIR)\log_rec.obj" \
	"$(INTDIR)\log_register.obj" \
	"$(INTDIR)\mp_alloc.obj" \
	"$(INTDIR)\mp_bh.obj" \
	"$(INTDIR)\mp_fget.obj" \
	"$(INTDIR)\mp_fopen.obj" \
	"$(INTDIR)\mp_fput.obj" \
	"$(INTDIR)\mp_fset.obj" \
	"$(INTDIR)\mp_method.obj" \
	"$(INTDIR)\mp_region.obj" \
	"$(INTDIR)\mp_register.obj" \
	"$(INTDIR)\mp_stat.obj" \
	"$(INTDIR)\mp_sync.obj" \
	"$(INTDIR)\mp_trickle.obj" \
	"$(INTDIR)\mut_tas.obj" \
	"$(INTDIR)\mutex.obj" \
	"$(INTDIR)\os_abs.obj" \
	"$(INTDIR)\os_alloc.obj" \
	"$(INTDIR)\os_dir.obj" \
	"$(INTDIR)\os_errno.obj" \
	"$(INTDIR)\os_fid.obj" \
	"$(INTDIR)\os_finit.obj" \
	"$(INTDIR)\os_fsync.obj" \
	"$(INTDIR)\os_handle.obj" \
	"$(INTDIR)\os_map.obj" \
	"$(INTDIR)\os_method.obj" \
	"$(INTDIR)\os_oflags.obj" \
	"$(INTDIR)\os_open.obj" \
	"$(INTDIR)\os_region.obj" \
	"$(INTDIR)\os_rename.obj" \
	"$(INTDIR)\os_root.obj" \
	"$(INTDIR)\os_rpath.obj" \
	"$(INTDIR)\os_rw.obj" \
	"$(INTDIR)\os_seek.obj" \
	"$(INTDIR)\os_sleep.obj" \
	"$(INTDIR)\os_spin.obj" \
	"$(INTDIR)\os_stat.obj" \
	"$(INTDIR)\os_tmpdir.obj" \
	"$(INTDIR)\os_type.obj" \
	"$(INTDIR)\os_unlink.obj" \
	"$(INTDIR)\qam.obj" \
	"$(INTDIR)\qam_auto.obj" \
	"$(INTDIR)\qam_conv.obj" \
	"$(INTDIR)\qam_method.obj" \
	"$(INTDIR)\qam_open.obj" \
	"$(INTDIR)\qam_rec.obj" \
	"$(INTDIR)\qam_stat.obj" \
	"$(INTDIR)\txn.obj" \
	"$(INTDIR)\txn_auto.obj" \
	"$(INTDIR)\txn_rec.obj" \
	"$(INTDIR)\txn_region.obj" \
	"$(INTDIR)\xa.obj" \
	"$(INTDIR)\xa_db.obj" \
	"$(INTDIR)\xa_map.obj"

"$(OUTDIR)\libdb30d.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "DB_DLL - Win32 Release"
# Name "DB_DLL - Win32 Debug"

!IF  "$(CFG)" == "DB_DLL - Win32 Release"

!ELSEIF  "$(CFG)" == "DB_DLL - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=\db\btree\btree_auto.c
DEP_CPP_BTREE=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\btree_auto.obj" : $(SOURCE) $(DEP_CPP_BTREE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_compare.c
DEP_CPP_BT_CO=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_compare.obj" : $(SOURCE) $(DEP_CPP_BT_CO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_conv.c
DEP_CPP_BT_CON=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_conv.obj" : $(SOURCE) $(DEP_CPP_BT_CON) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_cursor.c
DEP_CPP_BT_CU=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_cursor.obj" : $(SOURCE) $(DEP_CPP_BT_CU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_delete.c
DEP_CPP_BT_DE=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_delete.obj" : $(SOURCE) $(DEP_CPP_BT_DE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_open.c
DEP_CPP_BT_OP=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_open.obj" : $(SOURCE) $(DEP_CPP_BT_OP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_put.c
DEP_CPP_BT_PU=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_put.obj" : $(SOURCE) $(DEP_CPP_BT_PU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_rec.c
DEP_CPP_BT_RE=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_rec.obj" : $(SOURCE) $(DEP_CPP_BT_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_rsearch.c
DEP_CPP_BT_RS=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_rsearch.obj" : $(SOURCE) $(DEP_CPP_BT_RS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_search.c
DEP_CPP_BT_SE=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_search.obj" : $(SOURCE) $(DEP_CPP_BT_SE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_split.c
DEP_CPP_BT_SP=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_split.obj" : $(SOURCE) $(DEP_CPP_BT_SP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_stat.c
DEP_CPP_BT_ST=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_stat.obj" : $(SOURCE) $(DEP_CPP_BT_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\cxx\cxx_txn.cpp
DEP_CPP_CXX_T=\
	".\../include\cxx_int.h"\
	".\../include\db_cxx.h"\
	".\../include\queue.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\cxx_txn.obj" : $(SOURCE) $(DEP_CPP_CXX_T) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\cxx\cxx_except.cpp
DEP_CPP_CXX_E=\
	".\../include\cxx_int.h"\
	".\../include\db_cxx.h"\
	".\../include\queue.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\cxx_except.obj" : $(SOURCE) $(DEP_CPP_CXX_E) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\cxx\cxx_lock.cpp
DEP_CPP_CXX_L=\
	".\../include\cxx_int.h"\
	".\../include\db_cxx.h"\
	".\../include\queue.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\cxx_lock.obj" : $(SOURCE) $(DEP_CPP_CXX_L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\cxx\cxx_log.cpp
DEP_CPP_CXX_LO=\
	".\../include\cxx_int.h"\
	".\../include\db_cxx.h"\
	".\../include\queue.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\cxx_log.obj" : $(SOURCE) $(DEP_CPP_CXX_LO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\cxx\cxx_mpool.cpp
DEP_CPP_CXX_M=\
	".\../include\cxx_int.h"\
	".\../include\db_cxx.h"\
	".\../include\queue.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\cxx_mpool.obj" : $(SOURCE) $(DEP_CPP_CXX_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\cxx\cxx_table.cpp
DEP_CPP_CXX_TA=\
	".\../include\common_ext.h"\
	".\../include\cxx_int.h"\
	".\../include\db_cxx.h"\
	".\../include\queue.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\cxx_table.obj" : $(SOURCE) $(DEP_CPP_CXX_TA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\cxx\cxx_app.cpp
DEP_CPP_CXX_A=\
	".\../include\common_ext.h"\
	".\../include\cxx_int.h"\
	".\../include\db_cxx.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\cxx_app.obj" : $(SOURCE) $(DEP_CPP_CXX_A) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_auto.c
DEP_CPP_DB_AU=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_auto.obj" : $(SOURCE) $(DEP_CPP_DB_AU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_conv.c
DEP_CPP_DB_CO=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_conv.obj" : $(SOURCE) $(DEP_CPP_DB_CO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_dispatch.c
DEP_CPP_DB_DI=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_dispatch.obj" : $(SOURCE) $(DEP_CPP_DB_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_dup.c
DEP_CPP_DB_DU=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_dup.obj" : $(SOURCE) $(DEP_CPP_DB_DU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_overflow.c
DEP_CPP_DB_OV=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_overflow.obj" : $(SOURCE) $(DEP_CPP_DB_OV) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_pr.c
DEP_CPP_DB_PR=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_pr.obj" : $(SOURCE) $(DEP_CPP_DB_PR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_rec.c
DEP_CPP_DB_RE=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_rec.obj" : $(SOURCE) $(DEP_CPP_DB_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_ret.c
DEP_CPP_DB_RET=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_ret.obj" : $(SOURCE) $(DEP_CPP_DB_RET) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db.c
DEP_CPP_DB_C36=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db.obj" : $(SOURCE) $(DEP_CPP_DB_C36) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\common\db_byteorder.c
DEP_CPP_DB_BY=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_byteorder.obj" : $(SOURCE) $(DEP_CPP_DB_BY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\common\db_err.c
DEP_CPP_DB_ER=\
	".\../include\clib_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_err.obj" : $(SOURCE) $(DEP_CPP_DB_ER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\common\db_log2.c
DEP_CPP_DB_LO=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_log2.obj" : $(SOURCE) $(DEP_CPP_DB_LO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\dbm\dbm.c
DEP_CPP_DBM_C=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\dbm.obj" : $(SOURCE) $(DEP_CPP_DBM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_stat.c
DEP_CPP_HASH_=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_stat.obj" : $(SOURCE) $(DEP_CPP_HASH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_auto.c
DEP_CPP_HASH_A=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_auto.obj" : $(SOURCE) $(DEP_CPP_HASH_A) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_conv.c
DEP_CPP_HASH_C=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_conv.obj" : $(SOURCE) $(DEP_CPP_HASH_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_dup.c
DEP_CPP_HASH_D=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_dup.obj" : $(SOURCE) $(DEP_CPP_HASH_D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_func.c
DEP_CPP_HASH_F=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_func.obj" : $(SOURCE) $(DEP_CPP_HASH_F) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_page.c
DEP_CPP_HASH_P=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_page.obj" : $(SOURCE) $(DEP_CPP_HASH_P) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_rec.c
DEP_CPP_HASH_R=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_rec.obj" : $(SOURCE) $(DEP_CPP_HASH_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash.c
DEP_CPP_HASH_C4e=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash.obj" : $(SOURCE) $(DEP_CPP_HASH_C4e) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\lock\lock_util.c
DEP_CPP_LOCK_=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\lock_util.obj" : $(SOURCE) $(DEP_CPP_LOCK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\lock\lock_conflict.c
DEP_CPP_LOCK_C=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\lock_conflict.obj" : $(SOURCE) $(DEP_CPP_LOCK_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\lock\lock_deadlock.c
DEP_CPP_LOCK_D=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\lock_deadlock.obj" : $(SOURCE) $(DEP_CPP_LOCK_D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\lock\lock.c
DEP_CPP_LOCK_C56=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\lock.obj" : $(SOURCE) $(DEP_CPP_LOCK_C56) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_register.c
DEP_CPP_LOG_R=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_register.obj" : $(SOURCE) $(DEP_CPP_LOG_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_archive.c
DEP_CPP_LOG_A=\
	".\../include\clib_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_dispatch.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_archive.obj" : $(SOURCE) $(DEP_CPP_LOG_A) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_auto.c
DEP_CPP_LOG_AU=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_auto.obj" : $(SOURCE) $(DEP_CPP_LOG_AU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_compare.c
DEP_CPP_LOG_C=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_compare.obj" : $(SOURCE) $(DEP_CPP_LOG_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_findckp.c
DEP_CPP_LOG_F=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_findckp.obj" : $(SOURCE) $(DEP_CPP_LOG_F) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_get.c
DEP_CPP_LOG_G=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_get.obj" : $(SOURCE) $(DEP_CPP_LOG_G) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_put.c
DEP_CPP_LOG_P=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\clib_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_put.obj" : $(SOURCE) $(DEP_CPP_LOG_P) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_rec.c
DEP_CPP_LOG_RE=\
	".\../include\common_ext.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_rec.obj" : $(SOURCE) $(DEP_CPP_LOG_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log.c
DEP_CPP_LOG_C68=\
	".\../include\common_ext.h"\
	".\../include\db_dispatch.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log.obj" : $(SOURCE) $(DEP_CPP_LOG_C68) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_sync.c
DEP_CPP_MP_SY=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_sync.obj" : $(SOURCE) $(DEP_CPP_MP_SY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_fget.c
DEP_CPP_MP_FG=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_fget.obj" : $(SOURCE) $(DEP_CPP_MP_FG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_fopen.c
DEP_CPP_MP_FO=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_fopen.obj" : $(SOURCE) $(DEP_CPP_MP_FO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_fput.c
DEP_CPP_MP_FP=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_fput.obj" : $(SOURCE) $(DEP_CPP_MP_FP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_fset.c
DEP_CPP_MP_FS=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_fset.obj" : $(SOURCE) $(DEP_CPP_MP_FS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_region.c
DEP_CPP_MP_RE=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_region.obj" : $(SOURCE) $(DEP_CPP_MP_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_bh.c
DEP_CPP_MP_BH=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_bh.obj" : $(SOURCE) $(DEP_CPP_MP_BH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mutex\mutex.c
DEP_CPP_MUTEX=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mutex.obj" : $(SOURCE) $(DEP_CPP_MUTEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\txn\txn_rec.c
DEP_CPP_TXN_R=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\txn_rec.obj" : $(SOURCE) $(DEP_CPP_TXN_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\txn\txn_auto.c
DEP_CPP_TXN_A=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\txn_auto.obj" : $(SOURCE) $(DEP_CPP_TXN_A) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\txn\txn.c
DEP_CPP_TXN_C=\
	".\../include\common_ext.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\txn.obj" : $(SOURCE) $(DEP_CPP_TXN_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_recno.c
DEP_CPP_BT_REC=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_recno.obj" : $(SOURCE) $(DEP_CPP_BT_REC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_fsync.c
DEP_CPP_OS_FS=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_fsync.obj" : $(SOURCE) $(DEP_CPP_OS_FS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_oflags.c
DEP_CPP_OS_OF=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_oflags.obj" : $(SOURCE) $(DEP_CPP_OS_OF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_rpath.c
DEP_CPP_OS_RP=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_rpath.obj" : $(SOURCE) $(DEP_CPP_OS_RP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_rw.c
DEP_CPP_OS_RW=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_rw.obj" : $(SOURCE) $(DEP_CPP_OS_RW) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_stat.c
DEP_CPP_OS_ST=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_stat.obj" : $(SOURCE) $(DEP_CPP_OS_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_unlink.c
DEP_CPP_OS_UN=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_unlink.obj" : $(SOURCE) $(DEP_CPP_OS_UN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_alloc.c
DEP_CPP_OS_AL=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_alloc.obj" : $(SOURCE) $(DEP_CPP_OS_AL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hsearch\hsearch.c
DEP_CPP_HSEAR=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hsearch.obj" : $(SOURCE) $(DEP_CPP_HSEAR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\libdb.def

!IF  "$(CFG)" == "DB_DLL - Win32 Release"

!ELSEIF  "$(CFG)" == "DB_DLL - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\db\lock\lock_region.c
DEP_CPP_LOCK_R=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\lock_region.obj" : $(SOURCE) $(DEP_CPP_LOCK_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\dllmain.c

"$(INTDIR)\dllmain.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\libdb.rc

"$(INTDIR)\libdb.res" : $(SOURCE) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_curadj.c
DEP_CPP_BT_CUR=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_curadj.obj" : $(SOURCE) $(DEP_CPP_BT_CUR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_join.c
DEP_CPP_DB_JO=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_join.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_join.obj" : $(SOURCE) $(DEP_CPP_DB_JO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_iface.c
DEP_CPP_DB_IF=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_iface.obj" : $(SOURCE) $(DEP_CPP_DB_IF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_am.c
DEP_CPP_DB_AM=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_am.obj" : $(SOURCE) $(DEP_CPP_DB_AM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_spin.c
DEP_CPP_OS_SP=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_spin.obj" : $(SOURCE) $(DEP_CPP_OS_SP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_dir.c
DEP_CPP_OS_DI=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_dir.obj" : $(SOURCE) $(DEP_CPP_OS_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_fid.c
DEP_CPP_OS_FI=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_fid.obj" : $(SOURCE) $(DEP_CPP_OS_FI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_map.c
DEP_CPP_OS_MA=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_map.obj" : $(SOURCE) $(DEP_CPP_OS_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_seek.c
DEP_CPP_OS_SE=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_seek.obj" : $(SOURCE) $(DEP_CPP_OS_SE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_sleep.c
DEP_CPP_OS_SL=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_sleep.obj" : $(SOURCE) $(DEP_CPP_OS_SL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_abs.c
DEP_CPP_OS_AB=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_abs.obj" : $(SOURCE) $(DEP_CPP_OS_AB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_tmpdir.c
DEP_CPP_OS_TM=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_tmpdir.obj" : $(SOURCE) $(DEP_CPP_OS_TM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\env\env_region.c
DEP_CPP_ENV_R=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\env_region.obj" : $(SOURCE) $(DEP_CPP_ENV_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\env\db_shash.c
DEP_CPP_DB_SH=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_shash.obj" : $(SOURCE) $(DEP_CPP_DB_SH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\env\env_method.c
DEP_CPP_ENV_M=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\env_method.obj" : $(SOURCE) $(DEP_CPP_ENV_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\env\env_open.c
DEP_CPP_ENV_O=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\env_open.obj" : $(SOURCE) $(DEP_CPP_ENV_O) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\env\env_recover.c
DEP_CPP_ENV_RE=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\env_recover.obj" : $(SOURCE) $(DEP_CPP_ENV_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\env\db_salloc.c
DEP_CPP_DB_SA=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_salloc.obj" : $(SOURCE) $(DEP_CPP_DB_SA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_upgrade.c
DEP_CPP_BT_UP=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_upgrade.obj" : $(SOURCE) $(DEP_CPP_BT_UP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_method.c
DEP_CPP_BT_ME=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_method.obj" : $(SOURCE) $(DEP_CPP_BT_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_upgrade.c
DEP_CPP_HASH_U=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_upgrade.obj" : $(SOURCE) $(DEP_CPP_HASH_U) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_method.c
DEP_CPP_HASH_M=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_method.obj" : $(SOURCE) $(DEP_CPP_HASH_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_method.c
DEP_CPP_DB_ME=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_method.obj" : $(SOURCE) $(DEP_CPP_DB_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_trickle.c
DEP_CPP_MP_TR=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_trickle.obj" : $(SOURCE) $(DEP_CPP_MP_TR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_method.c
DEP_CPP_MP_ME=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_method.obj" : $(SOURCE) $(DEP_CPP_MP_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_register.c
DEP_CPP_MP_REG=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_register.obj" : $(SOURCE) $(DEP_CPP_MP_REG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_stat.c
DEP_CPP_MP_ST=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_stat.obj" : $(SOURCE) $(DEP_CPP_MP_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mp\mp_alloc.c
DEP_CPP_MP_AL=\
	".\../include\common_ext.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mp_alloc.obj" : $(SOURCE) $(DEP_CPP_MP_AL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_handle.c
DEP_CPP_OS_HA=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_handle.obj" : $(SOURCE) $(DEP_CPP_OS_HA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_method.c
DEP_CPP_OS_ME=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_method.obj" : $(SOURCE) $(DEP_CPP_OS_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_region.c
DEP_CPP_OS_RE=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_region.obj" : $(SOURCE) $(DEP_CPP_OS_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_root.c
DEP_CPP_OS_RO=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_root.obj" : $(SOURCE) $(DEP_CPP_OS_RO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\xa\xa_map.c
DEP_CPP_XA_MA=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\xa_map.obj" : $(SOURCE) $(DEP_CPP_XA_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\xa\xa_db.c
DEP_CPP_XA_DB=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\xa_db.obj" : $(SOURCE) $(DEP_CPP_XA_DB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\xa\xa.c
DEP_CPP_XA_Cd8=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\xa.obj" : $(SOURCE) $(DEP_CPP_XA_Cd8) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_errno.c
DEP_CPP_OS_ER=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_errno.obj" : $(SOURCE) $(DEP_CPP_OS_ER) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_open.c
DEP_CPP_OS_OP=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_open.obj" : $(SOURCE) $(DEP_CPP_OS_OP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\txn\txn_region.c
DEP_CPP_TXN_RE=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\txn_region.obj" : $(SOURCE) $(DEP_CPP_TXN_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\mutex\mut_tas.c
DEP_CPP_MUT_T=\
	"..\mutex\68K.gcc"\
	"..\mutex\alpha.gcc"\
	"..\mutex\parisc.gcc"\
	"..\mutex\sco.cc"\
	"..\mutex\sparc.gcc"\
	"..\mutex\x86.gcc"\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\mut_tas.obj" : $(SOURCE) $(DEP_CPP_MUT_T) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_meta.c
DEP_CPP_HASH_ME=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_meta.obj" : $(SOURCE) $(DEP_CPP_HASH_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\crdel_rec.c
DEP_CPP_CRDEL=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\crdel_rec.obj" : $(SOURCE) $(DEP_CPP_CRDEL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\crdel_auto.c
DEP_CPP_CRDEL_=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\crdel_auto.obj" : $(SOURCE) $(DEP_CPP_CRDEL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os\os_rename.c
DEP_CPP_OS_REN=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\os_jump.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_rename.obj" : $(SOURCE) $(DEP_CPP_OS_REN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\qam\qam_auto.c
DEP_CPP_QAM_A=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_dispatch.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\qam_auto.obj" : $(SOURCE) $(DEP_CPP_QAM_A) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\qam\qam.c
DEP_CPP_QAM_C=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mp.h"\
	".\../include\mp_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\qam.obj" : $(SOURCE) $(DEP_CPP_QAM_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\qam\qam_rec.c
DEP_CPP_QAM_R=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\qam_rec.obj" : $(SOURCE) $(DEP_CPP_QAM_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_meta.c
DEP_CPP_DB_MET=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\../include\txn.h"\
	".\../include\txn_auto.h"\
	".\../include\txn_ext.h"\
	".\../include\xa.h"\
	".\../include\xa_ext.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_meta.obj" : $(SOURCE) $(DEP_CPP_DB_MET) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_reclaim.c
DEP_CPP_DB_REC=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_reclaim.obj" : $(SOURCE) $(DEP_CPP_DB_REC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\hash\hash_reclaim.c
DEP_CPP_HASH_RE=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\hash_reclaim.obj" : $(SOURCE) $(DEP_CPP_HASH_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\qam\qam_conv.c
DEP_CPP_QAM_CO=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\qam_conv.obj" : $(SOURCE) $(DEP_CPP_QAM_CO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\btree\bt_reclaim.c
DEP_CPP_BT_RECL=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\bt_reclaim.obj" : $(SOURCE) $(DEP_CPP_BT_RECL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\db\db_upgrade.c
DEP_CPP_DB_UP=\
	"..\include\btree_auto.h"\
	"..\include\btree_ext.h"\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\hash_auto.h"\
	"..\include\hash_ext.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\btree.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\hash.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_upgrade.obj" : $(SOURCE) $(DEP_CPP_DB_UP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\log\log_method.c
DEP_CPP_LOG_M=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\log.h"\
	".\../include\log_auto.h"\
	".\../include\log_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\log_method.obj" : $(SOURCE) $(DEP_CPP_LOG_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\qam\qam_stat.c
DEP_CPP_QAM_S=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\qam_stat.obj" : $(SOURCE) $(DEP_CPP_QAM_S) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\qam\qam_open.c
DEP_CPP_QAM_O=\
	"..\include\crdel_auto.h"\
	"..\include\db_auto.h"\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_am.h"\
	".\../include\db_ext.h"\
	".\../include\db_page.h"\
	".\../include\db_shash.h"\
	".\../include\db_swap.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\lock.h"\
	".\../include\lock_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\qam_open.obj" : $(SOURCE) $(DEP_CPP_QAM_O) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\common\db_getlong.c
DEP_CPP_DB_GE=\
	".\../include\clib_ext.h"\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\db_getlong.obj" : $(SOURCE) $(DEP_CPP_DB_GE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\qam\qam_method.c
DEP_CPP_QAM_M=\
	"..\include\qam_auto.h"\
	"..\include\qam_ext.h"\
	".\../include\common_ext.h"\
	".\../include\db_page.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\qam.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\qam_method.obj" : $(SOURCE) $(DEP_CPP_QAM_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_type.c
DEP_CPP_OS_TY=\
	".\../include\queue.h"\
	".\../include\shqueue.h"\
	".\db_config.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_type.obj" : $(SOURCE) $(DEP_CPP_OS_TY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\db\os_win32\os_finit.c
DEP_CPP_OS_FIN=\
	".\../include\common_ext.h"\
	".\../include\debug.h"\
	".\../include\env_ext.h"\
	".\../include\mutex.h"\
	".\../include\mutex_ext.h"\
	".\../include\os.h"\
	".\../include\os_ext.h"\
	".\../include\queue.h"\
	".\../include\region.h"\
	".\../include\shqueue.h"\
	".\db.h"\
	".\db_config.h"\
	".\db_int.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\os_finit.obj" : $(SOURCE) $(DEP_CPP_OS_FIN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
