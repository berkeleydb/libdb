/*
 * Empty stub for db_config.h.
 *
 * Some harnesses #include a real libdb .c file directly (e.g. db_compint.c,
 * db_getlong.c) which begins with #include "db_config.h" / "db_int.h".  Those
 * generated headers pull in the entire tree, which CBMC does not need for the
 * self-contained function under test.  This empty stub (found via -Istubs)
 * satisfies the #include so the harness can provide exactly the handful of
 * typedefs / macros the target function actually uses.  See each harness's
 * top comment for the precise list of what it defines.
 */
