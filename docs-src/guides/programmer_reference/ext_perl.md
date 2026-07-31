---
title: "Using Berkeley DB with Perl"
api-name: "Using Berkeley DB with Perl"
source: docs/programmer_reference/ext_perl.html
---
## Using Berkeley DB with Perl

The original Perl module for Berkeley DB was DB_File, which was written to interface to Berkeley DB version 1.85. The newer Perl module for Berkeley DB is BerkeleyDB, which was written to interface to version 2.0 and subsequent releases. Because Berkeley DB version 2.X has a compatibility API for version 1.85, you can (and should!) build DB_File using version 2.X of Berkeley DB, although DB_File will still only support the 1.85 functionality.

DB_File is distributed with the standard Perl source distribution (look in the directory "ext/DB_File"). You can find both DB_File and BerkeleyDB on CPAN, the Comprehensive Perl Archive Network of mirrored FTP sites. The master CPAN site is <a href="ftp://ftp.funet.fi/" class="ulink" target="_top">ftp://ftp.funet.fi/</a>.

Versions of both BerkeleyDB and DB_File that are known to work correctly with each release of Berkeley DB are included in the distributed Berkeley DB source tree, in the subdirectories `perl.BerkeleyDB` and `perl.DB_File`. Each of those directories contains a `README` file with instructions on installing and using those modules.

The Perl interface is not maintained by Oracle. Questions about the DB_File and BerkeleyDB modules are best asked on the Usenet newsgroup comp.lang.perl.modules.
