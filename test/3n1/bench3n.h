/*
 * Copyright (c) 2011
 *	Donald D. Anderson.  All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted.
 * This software is provided 'as is' and any express or
 * implied warranties, including, but not limited to, the implied
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement, are disclaimed.
 */

#include <pthread.h>
#include <time.h>
typedef pthread_t thread_handle;
typedef pthread_mutex_t mutex_handle;
#define	mutex_init(mhandle)	pthread_mutex_init(mhandle, NULL)
#define	mutex_lock(mhandle)	pthread_mutex_lock(mhandle)
#define	mutex_unlock(mhandle)	pthread_mutex_unlock(mhandle)
#define thread_sleep(millis)


char *PROGRAM_NAME = (char *)"bench3n";

static void errexit(int ret, const char *who, const char *what) {
    fprintf(stderr, "%s: %s %s error: %s\n", PROGRAM_NAME,
      who, what, db_strerror(ret));
    exit(1);
}

#define CHK(call, who, what) do {               \
        int ret;                                \
        if ((ret = (call)) != 0) {               \
            errexit(ret, who, what);             \
        }                                        \
    } while(0)

// With auto-commit, we can just retry
#define DEADLOCK_RETRY(call, ntimes, who, what, ret) do {             \
        ret = 0;                                                      \
        int deadtries = 0;                                            \
        while (deadtries++ < ntimes) {                                \
            /*fprintf(stderr, "  %s ==> ", #call);*/                  \
            if ((ret = (call)) != DB_LOCK_DEADLOCK)                   \
                break;                                                \
            struct timespec ts;                                       \
            ts.tv_sec = 0;                                            \
            ts.tv_nsec = 1000 << deadtries;                           \
            nanosleep(&ts, NULL);                                     \
        }                                                             \
        /*fprintf(stderr, "%d\n", ret);*/                             \
    } while(0)

int
thread_start(void * (*func)(void *), void *param, thread_handle *result)
{
    pthread_t thread_id;
    int ret;

    if ((ret = pthread_create(&thread_id, NULL, func, param)) == 0) {
        *result = thread_id;
    }
    return (ret);
}

int
thread_join(thread_handle thandle)
{
    int ret;
    void *status;               /* ignored */

    return pthread_join(thandle, &status);
}
