/*
 * rule_tret_clobber.cocci -- flag t_ret being folded into ret without the
 * "&& ret == 0" guard.
 *
 * BDB's correct secondary-error idiom preserves the FIRST error:
 *     if ((t_ret = __foo(...)) != 0 && ret == 0)
 *             ret = t_ret;
 * Dropping the "&& ret == 0" clobbers an earlier failure with a later one:
 *     if ((t_ret = __foo(...)) != 0)
 *             ret = t_ret;          // BUG: loses the first error
 * or unconditionally:
 *     ret = t_ret;                  // BUG
 *
 * We match "if (COND) ret = t_ret;" where COND does NOT contain "ret == 0".
 * There are 760 "ret = t_ret;" statements and 12 known correct guards visible
 * to grep; the guarded ones bind ret==0 in the condition and are excluded.
 * Some legitimate patterns (ret assigned unconditionally on purpose) may match;
 * treated as advisory and kept OUT of the blocking baseline.  See README.md.
 *
 * Reported as identity transform tagged //@TRET_CLOBBER@.
 */

/* Unconditional / unguarded if without the ret==0 clause. */
@tret_bad_if@
expression E;
@@
  if (
- (t_ret = E) != 0
+ (t_ret = E) != 0 //@TRET_CLOBBER@
  )
  { ... ret = t_ret; ... }

@tret_bad_if2@
expression E;
@@
  if (
- (t_ret = E) != 0
+ (t_ret = E) != 0 //@TRET_CLOBBER@
  )
  ret = t_ret;
