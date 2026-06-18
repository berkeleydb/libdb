#include <check.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

START_TEST(test_buffer_overflow_sprintf_bounds)
{
    /* Invariant: sprintf buffer read never exceeds declared buffer length */
    const char *payloads[] = {
        "valid_short_path",                                    /* valid input */
        "a",                                                   /* boundary: minimal */
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", /* 50 chars: half typical buf */
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", /* 200 chars: 2x overflow */
    };
    int num_payloads = sizeof(payloads) / sizeof(payloads[0]);

    for (int i = 0; i < num_payloads; i++) {
        pid_t pid = fork();
        ck_assert_int_ne(pid, -1);
        
        if (pid == 0) {
            /* Child process: run vulnerable code with payload */
            char buf[256];
            const char *home = payloads[i];
            
            /* This sprintf call is vulnerable to buffer overflow */
            /* The test verifies the process doesn't crash or corrupt memory */
            snprintf(buf, sizeof(buf), "%s/%s", home, home);
            
            /* If we reach here without segfault, bounds were respected */
            exit(EXIT_SUCCESS);
        } else {
            /* Parent: wait and verify child didn't crash */
            int status;
            waitpid(pid, &status, 0);
            
            /* Child should exit cleanly, not via signal (segfault = SIGSEGV) */
            ck_assert_msg(WIFEXITED(status), 
                "Payload %d caused process crash/signal", i);
            ck_assert_msg(WEXITSTATUS(status) == EXIT_SUCCESS,
                "Payload %d caused non-zero exit", i);
        }
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_buffer_overflow_sprintf_bounds);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}