/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2002,2008 Oracle.  All rights reserved.
 *
 * $Id: Enhanced0.java,v 1.1 2008/02/07 17:12:32 mark Exp $
 */

package com.sleepycat.persist.test;

import static com.sleepycat.persist.model.Relationship.MANY_TO_ONE;

import com.sleepycat.persist.model.Entity;
import com.sleepycat.persist.model.PrimaryKey;
import com.sleepycat.persist.model.SecondaryKey;

/**
 * For running ASMifier -- before any enhancements.
 */
@Entity
class Enhanced0 {

    @PrimaryKey
    private String f1;

    @SecondaryKey(relate=MANY_TO_ONE)
    private int f2;
    @SecondaryKey(relate=MANY_TO_ONE)
    private String f3;
    @SecondaryKey(relate=MANY_TO_ONE)
    private String f4;

    private int f5;
    private String f6;
    private String f7;
}
