/*
 * rule_mutex_unbalanced.cocci -- flag a return that skips MUTEX_UNLOCK.
 *
 * MUTEX_LOCK(env, m) / MUTEX_UNLOCK(env, m) must balance on every path.  A
 * return between the lock and its unlock, with no intervening MUTEX_UNLOCK of
 * the same mutex and no goto to a cleanup label, leaks the lock -- a hang.
 *
 * BDB has 265 MUTEX_LOCK / 349 MUTEX_UNLOCK sites and heavy goto-based unlock,
 * so this is constrained to "lock, then return, with NO MUTEX_UNLOCK(env, m)
 * and NO goto in between".  Precision on this codebase is moderate; treated as
 * advisory and kept OUT of the blocking baseline.  See README.md.
 *
 * Reported as identity transform tagged //@MUTEX_UNBALANCED@.
 */

@mutex_unbalanced exists@
expression env, m, ret;
identifier lbl;
@@
  MUTEX_LOCK(env, m);
  ... when != MUTEX_UNLOCK(env, m)
      when != goto lbl;
- return (ret);
+ return (ret); //@MUTEX_UNBALANCED@
