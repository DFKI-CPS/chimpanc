/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2010.
 */

package org.dfki.utils;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: Oct 4, 2010
 * Time: 3:17:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class Collectionxx {
    public static <T> Boolean some(Collection<T> c, Test<T> test) {
        Boolean result = false;
        Iterator<T> it = c.iterator();
        while (it.hasNext() && !result) {
            result = test.check(it.next());
        }
        return result;
    }

    public static <T> Boolean all(Collection<T> c, Test<T> test) {
        Boolean result = true;
        Iterator<T> it = c.iterator();
        while (it.hasNext() && result) {
            result = test.check(it.next());
        }
        return result;
    }

    public static <T> T find(Collection<T> c, Test<T> test) {
        T result = null;
        Iterator<T> it = c.iterator();
        while (it.hasNext() && (result == null)) {
            T temp = it.next();
            if (test.check(temp)) result = temp;
        }
        return result;
    }

    public static <T> List<T> findAll(Collection<T> c, Test<T> test) {
        ArrayList<T> result = new ArrayList<T>();
        Iterator<T> it = c.iterator();
        while (it.hasNext() && (result == null)) {
            T temp = it.next();
            if (test.check(temp)) result.add(temp);
        }
        return result;
    }

    public static <T> boolean disjoint(Collection<T> c1, Collection<T> c2) {

        if (c1.size() > c2.size()) {
            Collection<T> c = c1;
            c1 = c2;
            c2 = c;
        }
        for (Iterator<T> iterator = c2.iterator(); iterator.hasNext();) {
            T o = iterator.next();
            if (c1.contains(o)) {
                return false;
            }
        }
        return true;
    }

    public static <T> boolean checkDuplicate(Collection<T> list) {
        HashSet<T> set = new HashSet<T>();
        Iterator<T> iterator = list.iterator();
        while (iterator.hasNext()) {
            T o = iterator.next();
            boolean val = set.add(o);
            if (val == false) {
                return false;
            }
        }
        return true;
    }

    public static <T> ArrayList<T> newList(T... elements) {
        ArrayList<T> l = new ArrayList<T>(elements.length);
        for (T e : elements) l.add(e);
        return l;
    }

    public static <T> Set<T> copy(Set<T> s) {
        Set<T> g = new HashSet<T>(s.size());
        g.addAll(s);
        return g;
    }

    public static <T> List<T> copy(List<T> s) {
        List<T> g = new ArrayList<T>(s.size());
        g.addAll(s);
        return g;
    }

    public static <T> Set<T> newSet(T... elements) {
            Set<T> l = new HashSet<T>(elements.length);
            for (T e : elements) l.add(e);
            return l;
        }

    public static <T> HashMap<T, T> newMap(T... elements) {
        if ((elements.length % 2) != 0) {
            System.err.println("Odd number of arguments to newMap");
        }
        HashMap<T, T> res = new HashMap<T, T>();
        for (int i = 0; i < elements.length; i = i + 2) {
            res.put(elements[i], elements[i + 1]);
        }
        return res;
    }

    public static <T> Set<T> intersection (Set<T> a, Set<T> b) {
        Set<T> result = new HashSet<T>();
        Iterator<T> ita = a.iterator();
        while (ita.hasNext()) {
            T e = ita.next();
            if (b.contains(e)) result.add(e);
        }
        return result;
    }

    public static <T> Set<T> union (Set<T> a, Set<T> b) {
        Set<T> result = new HashSet<T>(a.size()+b.size());
        result.addAll(a);
        result.addAll(b);
        return result;
    }

    public static <T> List<T> append (List<T> a, List<T> b) {
        List<T> result = new ArrayList<T>(a.size()+b.size());
        result.addAll(a);
        result.addAll(b);
        return result;
    }

    public static <T> Set<T> maximal (Collection<T> a,Comparator<T> comp) {
        Set<T> result = new HashSet<T>();
        Iterator<T> it = a.iterator();
        while (it.hasNext()) {
            T e = it.next();
            T greater = null;
            for(T o:a) { if (comp.compare(e,o)<0) { greater = o; break; } }
            if (greater==null) result.add(e);
        }
        return result;
    }

}
