---
title: "DPL Applications must be recompiled"
api-name: "DPL Applications must be recompiled"
source: docs/installation/upgrade_11gr2_51_dpl_recompile.html
---
## DPL Applications must be recompiled

Applications that use the Java interface's <span class="emphasis">*Direct Persistence Layer*</span> must be recompiled, due to a change in the return type of the setter methods in StoreConfig and EvolveConfig classes. The setter methods now return `this` instead of `void`.
