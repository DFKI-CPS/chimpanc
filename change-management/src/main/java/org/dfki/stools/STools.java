/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */

package org.dfki.stools;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.dfki.stools.similarityspec.SimilaritySpec;
import org.dfki.utils.Collectionxx;

import java.io.File;
import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 14.02.2011
 * Time: 16:43
 * To change this template use File | Settings | File Templates.
 */
public class STools extends HashMap<String, SToolInterface> {
	private static final long serialVersionUID = 1L;
	
	private Boolean sealed;
    private static final Log log = LogFactory.getLog(STools.class);
    private static final String predefinedequivspecs =
            "equivspec char { element <TEXT> { constituents { mandatory { <TEXT> } } } } \n"+
                    "equivspec charlist { element <TEXT> { constituents { ordered { <TEXT>[char] } } } }\n" +
                    "equivspec charlistlist { element <TEXT> { constituents { ordered { <TEXT>[charlist] } } } }\n" +
                    "equivspec charlistset { element <TEXT> { constituents { unordered { <TEXT>[charlist] } } } }\n" +
                    "equivspec charlistlistlist { element <TEXT> { constituents { ordered { <TEXT>[charlistlist] } } } }\n" +
                    "equivspec charlistsetlist { element <TEXT> { constituents { ordered { <TEXT>[charlistset] } } } }\n" +
                    "equivspec charlistlistset { element <TEXT> { constituents { unordered { <TEXT>[charlistlist] } } } }\n" +
                    "equivspec charlistsetset { element <TEXT> { constituents { unordered { <TEXT>[charlistset] } } } }\n" +
                    "equivspec charset { element <TEXT> { constituents { unordered { <TEXT>[char] } } } } \n" +
                    "equivspec charsetlist { element <TEXT> { constituents { ordered { <TEXT>[charset] } } } }\n" +
                    "equivspec charsetset { element <TEXT> { constituents { unordered { <TEXT>[charset] } } } }\n" +
                    "equivspec charsetlistlist { element <TEXT> { constituents { ordered { <TEXT>[charsetlist] } } } }\n" +
                    "equivspec charsetsetset { element <TEXT> { constituents { unordered { <TEXT>[charsetset] } } } }\n" +
                    "equivspec charsetlistlist { element <TEXT> { constituents { ordered { <TEXT>[charsetlist] } } } }\n" +
                    "equivspec charsetsetset { element <TEXT> { constituents { unordered { <TEXT>[charsetset] } } } }\n" +
                    "equivspec line { element <TEXT> { constituents { unordered { <TEXT> } } } }\n" +
                    "equivspec linelist { element <TEXT> { constituents { ordered { <TEXT>[line] } } } }\n" +
                    "equivspec lineset { element <TEXT> { constituents { unordered { <TEXT>[line] } } } }\n" +
                    "equivspec word { element <TEXT> { constituents { ordered { <TEXT> } } } }\n" +
                    "equivspec wordlist { element <TEXT> { constituents { ordered { <TEXT>[word] } } } }\n" +
                    "equivspec wordlistlist { element <TEXT> { constituents { ordered { <TEXT>[wordlist] } } } }\n" +
                    "equivspec wordlistset { element <TEXT> { constituents { unordered { <TEXT>[wordlist] } } } }\n" +
                    "equivspec wordset { element <TEXT> { constituents { unordered { <TEXT>[word] } } } }\n" +
                    "equivspec wordsetlist { element <TEXT> { constituents { ordered { <TEXT>[wordset] } } } }\n" +
                    "equivspec wordsetset { element <TEXT> { constituents { unordered { <TEXT>[wordset] } } } }";

    public STools() {
        super();
        sealed = false;
        for (SimilaritySpec spec : SimilaritySpec.fromString(predefinedequivspecs)) {
            addSimilaritySpec(spec);
        }
    }

    public STools(File... files) {
        super();
        sealed = false;
        for (SimilaritySpec spec : SimilaritySpec.fromString(predefinedequivspecs)) {
            addSimilaritySpec(spec);
        }
        for(File file:files) {
            for (SimilaritySpec spec : SimilaritySpec.fromFile(file)) {
                addSimilaritySpec(spec);
            }
        }
        normalizeSimilaritySpecs();
        sealed = true;
    }

    public STools(String equivspecs) {
        super();
        sealed = false;
        for (SimilaritySpec spec : SimilaritySpec.fromString(predefinedequivspecs)) {
            addSimilaritySpec(spec);
        }
        for (SimilaritySpec spec : SimilaritySpec.fromString(equivspecs)) {
            addSimilaritySpec(spec);
        }
        normalizeSimilaritySpecs();
        sealed = true;
    }

    public STools(List<SimilaritySpec> simspecs) {
        super();
        sealed = false;
        for (SimilaritySpec spec : SimilaritySpec.fromString(predefinedequivspecs)) {
            addSimilaritySpec(spec);
        }
        for (SimilaritySpec spec : simspecs) {
            addSimilaritySpec(spec);
        }
        normalizeSimilaritySpecs();
        sealed = true;
    }
    
    
    public void seal() {
        if (!sealed) {
            normalizeSimilaritySpecs();
            sealed = true;
        }
    }

    public void addSimilaritySpec(SimilaritySpec spec) {
        //log.debug(String.format("Adding spec '%s'",spec.getName()));
        if (!sealed) this.put(spec.getName(), new STool(spec, this));
        else log.error("Try to add similarityspec to sealed STools instance");
    }


    private Boolean isAcyclic() {
        Map<String, SimilaritySpec> specs = getSimilatySpecMapping();
        for (SimilaritySpec spec : specs.values()) {
            if (!isAcyclic(spec, new ArrayList<String>())) return false;
        }
        return true;
    }

    private Boolean isAcyclic(SimilaritySpec spec, List<String> visited) {
        for (String parent : spec.getParents()) {
            if (visited.contains(parent)) return false;
            else {
                List<String> newvisited = Collectionxx.newList(parent);
                newvisited.addAll(visited);
                if (!isAcyclic(getSTool(parent).getSimilaritySpec(), newvisited))
                    return false;
            }
        }
        return true;
    }


    private void normalizeSimilaritySpecs() {
        assert (!sealed);
        if (isAcyclic()) {
            Map<String, SimilaritySpec> mapping = getSimilatySpecMapping();
            for (SimilaritySpec spec : mapping.values()) {
                expandSimilaritySpec(spec,mapping);
            }
        } else {
            log.error("Cannot normalize cyclic similarityspec structure");
        }
    }
    
    private void expandSimilaritySpec (SimilaritySpec spec,Map<String, SimilaritySpec> mapping) {
        for(String parent:spec.getParents()) {
            SimilaritySpec parentspec = mapping.get(parent);
            if (parentspec==null) log.error("Could not find similarityspec "+parent);
            expandSimilaritySpec(parentspec,mapping);
            spec.includeSimilaritySpec(parentspec);
        }
        spec.setParents(new HashSet<String>(0));
    }

    private HashMap<String, SimilaritySpec> getSimilatySpecMapping() {
        HashMap<String, SimilaritySpec> specs = new HashMap<String, SimilaritySpec>();
        for (String name : this.keySet()) {
            specs.put(name, this.get(name).getSimilaritySpec());
        }
        return specs;
    }


    public SToolInterface getSTool(String name) {
        if (containsKey(name)) return get(name);
        else {
            log.error(String.format("Could not find stool for name '%s'",name));
            return null;
        }
    }


}
