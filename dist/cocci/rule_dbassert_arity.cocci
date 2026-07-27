/*
 * rule_dbassert_arity.cocci -- flag DB_ASSERT() called with one argument.
 *
 * DB_ASSERT is defined (src/dbinc/debug.h) as:
 *     #define DB_ASSERT(env, e) ...
 * i.e. it always needs the ENV handle AND the asserted expression.  A
 * one-argument call DB_ASSERT(expr) compiles to nothing under DIAGNOSTIC
 * (macro-arity mismatch is often silently accepted by cpp) and silently
 * drops the assertion -- a real, dangerous mistake.
 *
 * Coccinelle is a good fit: this is pure call-shape matching.  Reported as an
 * identity transform tagged //@DBASSERT_ARITY@.  The tree is currently clean
 * (0 instances); this guards against new one-arg calls.
 *
 * Convention check / early warning -- see README.md.
 */

@dbassert_one_arg@
expression e;
@@
- DB_ASSERT(e)
+ DB_ASSERT(e) //@DBASSERT_ARITY@
