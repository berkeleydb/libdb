/*
 * rule_malloc_leak.cocci -- flag __os_malloc with an early return that skips
 * cleanup on the same path (a leak).
 *
 * BDB's error-cleanup convention is goto-based:
 *     if ((ret = __os_malloc(env, n, &p)) != 0) goto err;
 *     ... err: __os_free(env, p);
 * A leak looks like:
 *     __os_malloc(env, n, &p);
 *     ... (no __os_free(p), no goto)
 *     return (ret);            // p leaked
 *
 * This is the WEAKEST rule of the set: BDB has 169 __os_malloc / 680 __os_free
 * sites and pervasive goto-err cleanup, so a naive "malloc without free before
 * return" flags almost every legitimate allocation.  We constrain it to the
 * narrow shape "malloc into &p, then return with NO intervening __os_free(...,p)
 * and NO goto of any kind".  Even so, precision on this codebase is low;
 * treated as EXPERIMENTAL / advisory only and kept OUT of the blocking
 * baseline.  See README.md.
 *
 * Reported as identity transform tagged //@MALLOC_LEAK@ (EXPERIMENTAL).
 */

@malloc_leak exists@
expression env, sz, p, ret;
identifier lbl;
@@
  ret = __os_malloc(env, sz, &p);
  ... when != __os_free(env, p)
      when != goto lbl;
- return (ret);
+ return (ret); //@MALLOC_LEAK@
