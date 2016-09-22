/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools.similarityspec;

import org.antlr.runtime.tree.Tree;
import org.dfki.stools.ISElement;

import scala.Tuple2;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import static org.dfki.stools.SimilaritySpecLexer.*;

/**
* Created by IntelliJ IDEA.
* User: autexier
* Date: 16.02.2011
* Time: 13:10
* To change this template use File | Settings | File Templates.
*/


public class ElementSimilaritySpec {
    public String ns;
    public String tagname;
    public AnnotationSimilaritySpec annotationSimSpec;
    public SubElementSimilaritySpec subelementSimSpec;

    public ElementSimilaritySpec(String ns,
                                 String name,
                                 AnnotationSimilaritySpec annotationSimSpec,
                                 SubElementSimilaritySpec subelementSimSpec) {
        this.ns = ns;
        this.tagname = name;
        this.annotationSimSpec = annotationSimSpec;
        this.subelementSimSpec = subelementSimSpec;
    }

    public ElementSimilaritySpec(Tree tree, SimilaritySpec spec) {
        annotationSimSpec = new AnnotationSimilaritySpec();
        subelementSimSpec = new SubElementSimilaritySpec();
        if (tree.getType()== ELEMENT || tree.getType() == DEFAULTSPEC) {
            if (tree.getType() == DEFAULTSPEC) { this.ns = null;  this.tagname = null; }
           for(int i=0;i<tree.getChildCount();i++) {
               Tree child = tree.getChild(i);
               switch (child.getType()) {
                   case PREFIXEDNAME:
                        this.ns = child.getChild(0).getText();
                        this.tagname = child.getChild(1).getText();
                       break;
                   case UNPREFIXEDNAME:
                       this.ns = "";
                       this.tagname = child.getChild(0).getText();
                       break;
                   case TEXTNODE:
                       this.ns = "";
                       this.tagname = "<TEXT>";
                   case COMMENTNODE:
                       this.ns = "";
                       this.tagname = child.getText();
                    break;
                   case ALTERNATIVES:
                       if (child.getChildCount()>1) {
                           System.err.println("Cannot handle alternatives yet");
                       }
                       if (child.getChildCount()>0) {
                          if (child.getChild(0).getType()==ENTRY) {
                              Tuple2<AnnotationSimilaritySpec,SubElementSimilaritySpec> p = parseEntry(child.getChild(0),spec);
                              annotationSimSpec = p._1();
                              subelementSimSpec = p._2();
                          }
                       }

               }
           }
        }
    }

    private Tuple2<AnnotationSimilaritySpec,SubElementSimilaritySpec> parseEntry (Tree tree, SimilaritySpec spec) {
        AnnotationSimilaritySpec aspecs = new AnnotationSimilaritySpec();
        SubElementSimilaritySpec especs = new SubElementSimilaritySpec();

        for(int i=0;i<tree.getChildCount();i++) {
            Tree child = tree.getChild(i);
            switch (child.getType()) {
                case ANNOTATIONS:
                    aspecs = new AnnotationSimilaritySpec(child,spec);
                    break;
                case CONSTITUENTS:
                    especs = new SubElementSimilaritySpec(child,spec);
                    break;
            }
        }
        return new Tuple2<AnnotationSimilaritySpec, SubElementSimilaritySpec>(aspecs,especs);
    }

    public String getSubElementEquivSpec (ISElement e, String parentequivspec) {
        return subelementSimSpec.getSubElementEquivSpec(e,parentequivspec);
    }

    public Boolean isValid() {
        return annotationSimSpec.isValid() && subelementSimSpec.isValid();
    }

    public ArrayList<String> getAllAnnotationNames() {
        return annotationSimSpec.getAllAnnotationNames();
    }

    public ArrayList<String> getAllTagNames() {
        return subelementSimSpec.getAllTagNames();
    }

    public String toString() {
        String res = annotationSimSpec.toString();
        res = res + "\n";
        res = res + subelementSimSpec.toString();
        return res;
    }

    public Set<AnnotationNameSpec> relevantAttributeSpecs () {
        Set<AnnotationNameSpec> result = new HashSet<AnnotationNameSpec>();
        for(AnnotationSpec spec:annotationSimSpec.checkannotations) {
            if (spec instanceof AnnotationNameSpec) result.add((AnnotationNameSpec) spec);
        }
        for(AnnotationSpec spec:annotationSimSpec.mandatory) {
            if (spec instanceof AnnotationNameSpec) result.add((AnnotationNameSpec) spec);
        }
        return result;
    }


}
