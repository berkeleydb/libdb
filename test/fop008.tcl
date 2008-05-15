# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005,2008 Oracle.  All rights reserved.
#
# $Id: fop008.tcl,v 12.6 2008/01/08 20:58:53 bostic Exp $
#
# TEST	fop008
# TEST	Test file system operations on named in-memory databases.
# TEST	Combine two ops in one transaction.
proc fop008 { method args } {
	eval {fop006 $method 1} $args
}



