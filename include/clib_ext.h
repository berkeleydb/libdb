/* DO NOT EDIT: automatically built by dist/distrib. */
#ifndef _clib_ext_h_
#define _clib_ext_h_
#ifndef HAVE_GETCWD
char *getcwd __P((char *, size_t));
#endif
#ifndef HAVE_GETOPT
int getopt __P((int, char * const *, const char *));
#endif
#ifndef HAVE_MEMCMP
int memcmp __P((const void *, const void *, size_t));
#endif
#ifndef HAVE_MEMCPY
void *memcpy __P((void *, const void *, size_t));
#endif
#ifndef HAVE_MEMMOVE
void *memmove __P((void *, const void *, size_t));
#endif
#ifndef HAVE_RAISE
int raise __P((int));
#endif
#ifndef HAVE_SNPRINTF
#ifdef __STDC__
int snprintf __P((char *, size_t, const char *, ...));
#else
int snprintf();
#endif
#endif
#ifndef HAVE_STRERROR
char *strerror __P((int));
#endif
#ifndef HAVE_VSNPRINTF
int vsnprintf();
#endif
#endif /* _clib_ext_h_ */
