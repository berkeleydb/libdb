---
title: "Locking and non-Berkeley DB applications"
api-name: "Locking and non-Berkeley DB applications"
source: docs/programmer_reference/lock_nondb.html
---
## Locking and non-Berkeley DB applications

The Lock subsystem is useful outside the context of Berkeley DB. It can be used to manage concurrent access to any collection of either ephemeral or persistent objects. That is, the lock region can persist across invocations of an application, so it can be used to provide long-term locking (for example, conference room scheduling).

In order to use the locking subsystem in such a general way, the applications must adhere to a convention for identifying objects and lockers. Consider a conference room scheduling problem, in which there are three conference rooms scheduled in half-hour intervals. The scheduling application must then select a way to identify each conference room/time slot combination. In this case, we could describe the objects being locked as bytestrings consisting of the conference room name, the date when it is needed, and the beginning of the appropriate half-hour slot.

Lockers are 32-bit numbers, so we might choose to use the User ID of the individual running the scheduling program. To schedule half-hour slots, all the application needs to do is issue a <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> call for the appropriate locker/object pair. To schedule a longer slot, the application needs to issue a <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> call, with one <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> operation per half-hour — up to the total length. If the <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> call fails, the application would have to release the parts of the time slot that were obtained.

To cancel a reservation, the application would make the appropriate <a href="../../api/c/lockput.md" class="olink">DB_ENV-&gt;lock_put()</a> calls. To reschedule a reservation, the <a href="../../api/c/lockget.md" class="olink">DB_ENV-&gt;lock_get()</a> and <a href="../../api/c/lockput.md" class="olink">DB_ENV-&gt;lock_put()</a> calls could all be made inside of a single <a href="../../api/c/lockvec.md" class="olink">DB_ENV-&gt;lock_vec()</a> call. The output of <a href="../../api/c/lockstat.md" class="olink">DB_ENV-&gt;lock_stat()</a> could be post-processed into a human-readable schedule of conference room use.
