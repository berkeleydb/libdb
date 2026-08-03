---
title: "Integrating Changes into the Berkeley DB Source Code"
api-name: "Integrating Changes into the Berkeley DB Source Code"
source: docs/porting/sourceintegrate.html
---
## Integrating Changes into the Berkeley DB Source Code

Once you have have completed your port, send all code changes that you made as "diff" files to Berkeley DB development for review.

Exactly how you integrate changes into the Berkeley DB source code varies depending on whether or not Oracle has agreed to support Berkeley DB on the new platform:

At this point, when Oracle has agreed to support Berkeley DB on the new platform:

1.  Berkeley DB development reviews the changes, makes the changes (with modifications if necessary) in the source code, creates another snapshot which includes the new platform-specific changes, and sends you the new snapshot.

2.  On receipt of the new snapshot, recompile this new snapshot. If the new snapshot compiles correctly without any changes, test the port again.
