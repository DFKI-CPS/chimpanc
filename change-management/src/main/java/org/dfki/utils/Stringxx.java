/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2010.
 */

package org.dfki.utils;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: Nov 4, 2010
 * Time: 12:44:09 PM
 * To change this template use File | Settings | File Templates.
 */
public class Stringxx {
    public static String tabulate(String str,int n) {
        String tab = "";
        for(int i=0;i<n;i++) tab = tab + " ";
        String res = "";
        if (str.contains("\n")) {
            for(String s:str.split("\n")) {
                res = res + tab + s + "\n";
            }
        } else res = tab + str;
        return res;
    }
}
