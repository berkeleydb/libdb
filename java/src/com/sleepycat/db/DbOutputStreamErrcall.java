/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)DbOutputStreamErrcall.java	10.1 (Sleepycat) 11/10/97
 */

package com.sleepycat.db;
import java.io.OutputStream;
import java.io.IOException;

/**
 *
 * @author Donald D. Anderson
 */
public class DbOutputStreamErrcall implements DbErrcall
{
    DbOutputStreamErrcall(OutputStream stream)
    {
        this.stream_ = stream;
    }

    // methods
    //
    public void errcall(String prefix, String buffer)
    {
        try {
            stream_.write(prefix.getBytes());
            stream_.write((new String(": ")).getBytes());
            stream_.write(buffer.getBytes());
            stream_.write((new String("\n")).getBytes());
        }
        catch (IOException e) {
            // nothing much we can do.
        }
    }

    // private data
    //
    private OutputStream stream_;
}

// end of DbOutputStreamErrcall.java
