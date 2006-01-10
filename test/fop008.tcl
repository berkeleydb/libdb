# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005
#	Sleepycat Software.  All rights reserved.
#
# $Id: fop008.tcl,v 12.1 2005/09/28 18:17:02 carol Exp $
#
# TEST	fop008
# TEST	Test file system operations on named in-memory databases.
# TEST	Combine two ops in one transaction.
proc fop008 { method args } {
	eval {fop006 $method 1} $args
}



