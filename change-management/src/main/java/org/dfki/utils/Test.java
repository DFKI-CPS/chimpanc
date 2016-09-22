/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2010.
 */

package org.dfki.utils;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: Oct 4, 2010
 * Time: 3:16:20 PM
 * To change this template use File | Settings | File Templates.
 */

public abstract class Test<T> {
    protected T base;

    protected Object others;

    public Test (T o) { base = o; others = null; }


    protected Test(T o,Object others) {
        this.others = others;
        base = o;
    }

    abstract public Boolean check(T t);
}
