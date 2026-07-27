/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * sim_os.h --
 *	Declarations of the DST I/O hooks that the __os_* layer calls.
 *	Included by src/os/os_rw.c and os_fsync.c ONLY under #ifdef
 *	HAVE_DST, so a production build never sees it and the auto-built
 *	os_ext.h is left untouched (these are DST-internal, not part of the
 *	public os API surface).
 */

#ifndef _DB_SIM_OS_H_
#define _DB_SIM_OS_H_

#if defined(__cplusplus)
extern "C" {
#endif

uint64_t db_sim_fkey __P((DB_FH *));
void __db_sim_io_write_hook __P((DB_FH *, u_int32_t));
void __db_sim_io_write_off_hook __P((DB_FH *, u_int64_t));
int __db_sim_io_write_fault_hook __P((DB_FH *, u_int32_t));
void __db_sim_io_presnapshot_hook __P((DB_FH *, u_int64_t, u_int32_t));
void __db_sim_io_sync_hook __P((DB_FH *));
void __db_sim_io_read_hook __P((DB_FH *, u_int64_t, void *, size_t));
void __db_sim_io_latency_hook __P((void));

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_SIM_OS_H_ */
