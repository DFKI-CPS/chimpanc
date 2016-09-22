/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki;

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 19.01.2011
 * Time: 10:30
 * To change this template use File | Settings | File Templates.
 */


public class PersonalNamespaceContext implements NamespaceContext {

    public Map<String, String> namespaces = null;

    public PersonalNamespaceContext(Map<String, String> ns) {
        namespaces = new HashMap<String, String>(ns.size());
        namespaces.putAll(ns);
    }
    
    public String toString() {
        String result = null;
        if (namespaces.isEmpty()) result = "PersonalNamespaceContext()";
        else {
            for (Map.Entry<String, String> e : namespaces.entrySet()) {
                if (result == null) result = String.format("PersonalNamespaceContext(%s=%s", e.getKey(), e.getValue());
                else result = result + String.format(",%s=%s", e.getKey(), e.getValue());
            }
            result = result + ")";
        }
        return result;
    }

    public String getNamespaceURI(String prefix) {
        String result = XMLConstants.NULL_NS_URI;
        if (prefix == null) throw new NullPointerException("Null prefix");
        /** else if (defaultprefix != null && namespaces.containsKey("") && prefix.equals(defaultprefix))
         return namespaces.get("");   **/
        else if (namespaces.containsKey(prefix)) result = namespaces.get(prefix);
        else if (XMLConstants.XMLNS_ATTRIBUTE.equals(prefix)) result = XMLConstants.XMLNS_ATTRIBUTE_NS_URI;
        else if (XMLConstants.XML_NS_PREFIX.equals(prefix)) result = XMLConstants.XML_NS_URI;
        //Debug.println(prefix+" -> "+result);
        return result;
    }

    public String getPrefix(String uri) {
        String result = null;
        if (null == uri) throw new NullPointerException("Null uri");
        else if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(uri)) result = XMLConstants.XMLNS_ATTRIBUTE;
        else if (XMLConstants.XML_NS_URI.equals(uri)) result = XMLConstants.XML_NS_PREFIX;
        else {
            for (Map.Entry<String, String> e : namespaces.entrySet()) {
                if (e.getValue().equals(uri)) {
                    result = e.getKey();
                    break;
                }
            }
        }
        if (result != null) return result;
        else if (uri.equals(XMLConstants.NULL_NS_URI)) return "";
        else {
            System.err.println("Unknown uri " + uri);
            //throw new UnsupportedOperationException();
            return uri;
        }
    }

    // This method isn't necessary for XPath processing either.
    public Iterator getPrefixes(String uri) {
        throw new UnsupportedOperationException();
    }


}
