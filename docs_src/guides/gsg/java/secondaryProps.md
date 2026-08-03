---
title: "Secondary Database Properties"
api-name: "Secondary Database Properties"
source: docs/gsg/JAVA/secondaryProps.html
---
## Secondary Database Properties

Secondary databases accept `SecondaryConfig` objects. `SecondaryConfig` is a subclass of `DatabaseConfig`, so it can manage all of the same properties as does `DatabaseConfig`. See <a href="dbprops.md" class="xref" title="Database Properties">Database Properties</a> for more information.

In addition to the `DatabaseConfig` properties, `SecondaryConfig` also allows you to manage the following properties:

- `SecondaryConfig.setAllowPopulate()`

  If true, the secondary database can be auto-populated. This means that on open, if the secondary database is empty then the primary database is read in its entirety and additions/modifications to the secondary's records occur automatically.

- `SecondaryConfig.setKeyCreator()`

  Identifies the key creator object to be used for secondary key creation. See <a href="keyCreator.md" class="xref" title="Implementing Key Creators">Implementing Key <span>Creators</span></a> for more information.
