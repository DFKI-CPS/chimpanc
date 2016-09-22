/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import java.util.*;

import org.dfki.stools.ISElement;
import org.dfki.stools.SElementOrTuple;
import org.dfki.stools.SElementTuple;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 16.02.2011
 * Time: 13:33
 * To change this template use File | Settings | File Templates.
 */
public class ElementSpecs extends ArrayList<ElementSpec> {
	private static final long serialVersionUID = 1L;

	public ElementSpecs() {
        super();
    }

    public ElementSpecs(ElementSpec... s) {
        super();
        for (ElementSpec e : s) add(e);
    }

    public Boolean contains(ElementNameOrTupleSpec s) {
        if (s instanceof ElementNameSpec) {
            ElementNameSpec as = (ElementNameSpec) s;

            for (ElementSpec e : this) {
                if (e instanceof ElementNameSpec) {
                    ElementNameSpec es = (ElementNameSpec) e;
                    //System.err.println(String.format("es %s %s %s %s ",es,es.getNamespace(),es.getName(),es.getEquivSpec()));
                    //System.err.println(String.format("as %s %s %s %s ",as,as.getNamespace(),as.getName(),as.getEquivSpec()));
                    if (es.getNamespace().equals(as.getNamespace()) &&
                            es.getName().equals(as.getName()) &&
                            es.getEquivSpec().equals(as.getEquivSpec())) return true;
                } else if (e instanceof ElementTupleSpec) {
                    if (((ElementTupleSpec) e).contains(as)) return true;
                }
            }
        }
        // TODO: Eventually check for empty intersections in tuple specs
        return false;
    }

    public void insert(ElementNameOrTupleSpec as) {
        if (!contains(as)) { 
            //System.err.println("Adding "+as);
            add(as);
        }
    }

    public void insertAll(List<ElementNameSpec> asl) {
        for (ElementNameSpec as : asl) insert(as);
    }

    public Map<ElementSpec,List<SElementOrTuple<?>>> projection(List<ISElement<?>> l) {
        Map<ElementSpec,List<SElementOrTuple<?>>> result = new HashMap<ElementSpec, List<SElementOrTuple<?>>>();
        Iterator<ElementSpec> it = this.iterator();
        while (it.hasNext()) result.put(it.next(),new ArrayList<SElementOrTuple<?>>());
        ArrayList<SElementOrTuple<?>> res = new ArrayList<SElementOrTuple<?>>();
        for (int i = 0; i < l.size(); i++) {
            it = this.iterator();
            Boolean matched = false;
            while (it.hasNext() && !matched) {
                ElementSpec s = it.next();
                if (s instanceof ElementNameSpec) {
                    ElementNameSpec e = (ElementNameSpec) s;
                    ISElement<?> t = e.match(l, i);
                    if (t != null) {
                        //System.out.println(String.format("%s matches %s",e,l.get(i)));
                        result.get(s).add(t);
                        res.add(t);
                        matched = true;
                    }
                } else if (s instanceof ElementTupleSpec) {
                    ElementTupleSpec e = (ElementTupleSpec) s;
                    SElementTuple<?> t = e.match(l, i);
                    if (t != null) {
                        result.get(s).add(t);
                        res.add(t);
                        i = i + t.size() - 1;
                        matched = true;
                    }
                }
            }
        }
        result = new HashMap<ElementSpec, List<SElementOrTuple<?>>>(1);
        result.put((this.isEmpty() ? null:this.get(0)),res);
        //System.out.println("Projection "+(this.isEmpty() ? null:this.get(0))+": "+res);
        return result;
    }
    
    public Boolean match(ISElement<?> se, Boolean adjustequivspec) {

    Iterator<ElementSpec> it = this.iterator();
        Boolean matched = false;
        while (it.hasNext() && !matched) {
            ElementSpec s = it.next();
            if (s instanceof ElementNameSpec) {
                ElementNameSpec e = (ElementNameSpec) s;
                ISElement<?> t = e.match(se,adjustequivspec);
                if (t != null) {
                    matched = true;
                }
            }
        }
        return matched;
    }
}
