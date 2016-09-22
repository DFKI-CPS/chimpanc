/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import javax.xml.XMLConstants;

import org.dfki.stools.ISElement;

import java.util.ArrayList;
import java.util.List;

/**
* Created by IntelliJ IDEA.
* User: autexier
* Date: 16.02.2011
* Time: 13:33
* To change this template use File | Settings | File Templates.
*/
public class ElementNameSpec extends ElementNameOrTupleSpec {
    protected SimilaritySpec spec;
    protected String ns;
    protected String tagname;
    protected String equivspec;

    public ElementNameSpec(String ns, String n, String eqspec, SimilaritySpec spec) {
        tagname = n;
        this.ns = ns;
        if (this.ns==null) this.ns = "";
        this.equivspec = (eqspec!=null) ? eqspec : "";
        this.spec = spec;
    }

    public String getNamespace() {
        return ns;
    }

    public String getName() {
        return tagname;
    }

    public String getEquivSpec() {
        return equivspec;
    }

    public List<ISElement<?>> eval(List<ISElement<?>> al) {
        ArrayList<ISElement<?>> res = new ArrayList<ISElement<?>>();
        for (int i = 0; i < al.size(); i++) {
            ISElement<?> e = match(al, i);
            if (e != null) res.add(e);
        }
        return res;
    }


    public ISElement<?> match(List<ISElement<?>> al, int pos) {
        ISElement<?> e = al.get(pos);
        if (e.getNamespace().equals(getNamespace()) &&
                e.getType().equals(getName())) {
            // Ajdusting the specific equivspec names for the recursion

            if (!equivspec.isEmpty()) {
                //System.out.println("Setting element "+e+" equivspec to '"+equivspec+"'");
                e.setEquivSpec(equivspec);
            }
            return e;
        } else return null;
    }


    public ISElement<?> match(ISElement<?> e, Boolean adjustequivspec) {
        if (e.getNamespace().equals(getNamespace()) &&
                e.getType().equals(getName())) {
            // Ajdusting the specific equivspec names for the recursion
            if (adjustequivspec && !equivspec.isEmpty()) {
                //System.out.println("Setting element "+e+" equivspec to "+equivspec);
                e.setEquivSpec(equivspec);
            }

            return e;
        } else return null;
    }

    public List<String> getAllTagNames() {
        ArrayList<String> res = new ArrayList<String>();
        if (!ns.isEmpty()) res.add(ns + ":" + tagname);
        else res.add(tagname);
        return res;
    }

    public Boolean isValid() {
        return true;
    }

    public String toString() {
        String result = tagname;
        if (!ns.isEmpty() && !ns.equals(XMLConstants.NULL_NS_URI)) {
            if (!spec.namespaceContext().getPrefix(ns).equals("")) {
                result = spec.namespaceContext().getPrefix(ns) + ":" + result;
            }
            //result = ns+":"+result;
        }
        if (!equivspec.isEmpty()) result = result + "[" + equivspec + "]";
        return result;
    }

}
