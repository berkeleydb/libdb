---
title: "Modifying the Tests"
api-name: "Modifying the Tests"
source: docs/porting/modifytest.html
---
## Modifying the Tests

There should be no need to make modifications to the tests. However, in a few situations, the test hardware may have some constraints (for example, small amount of memory) which may cause certain tests (which expect more memory) to fail. In these rare situations, you may need to modify the tests to work within the constraints of the platform.

When Oracle has agreed to support Berkeley DB on the new platform, submit any proposed test changes for review and approval by Oracle Engineering before running the tests.
