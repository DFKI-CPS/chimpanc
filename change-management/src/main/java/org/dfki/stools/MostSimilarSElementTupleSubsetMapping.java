/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2012.
 */

package org.dfki.stools;

import java.util.List;

import org.dfki.utils.MostSimilarSubsetMapping;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 14.02.2011
 * Time: 11:31
 * To change this template use File | Settings | File Templates.
 */

public class MostSimilarSElementTupleSubsetMapping extends MostSimilarSubsetMapping<SElementTuple<?>> {
    private List<SElementTuple<?>> X;
    private List<SElementTuple<?>> Y;
    private STools manager;

    public MostSimilarSElementTupleSubsetMapping () {}

    public MostSimilarSElementTupleSubsetMapping(STools m, List<SElementTuple<?>> l, List<SElementTuple<?>> r) {
        super();
        X = l;
        Y = r;
        manager = m;
    }

    @Override
    public void setX(List<SElementTuple<?>> xl) {
        X = xl;
    }

    @Override
    public void setY(List<SElementTuple<?>> yl) {
        Y = yl;
    }

    @Override
    public void initFrom(MostSimilarSubsetMapping<SElementTuple<?>> other) {
        if (other instanceof MostSimilarSElementTupleSubsetMapping) {
            manager = ((MostSimilarSElementTupleSubsetMapping) other).manager;
        }
    }

    @Override
    protected int sizeOfX() {
        return X.size();
    }

    @Override
    protected int sizeOfY() {
        return Y.size();
    }

    @Override
    protected SElementTuple<?> valueOfX(int index) {
        return X.get(index);
    }

    @Override
    protected SElementTuple<?> valueOfY(int index) {
        return Y.get(index);
    }

    @Override
         protected double similarity(SElementTuple<?> x1, SElementTuple<?> y1) {
            if (x1.compatible(y1)) {
                double sim = 0.0;
                for (int i = 0; i < x1.size(); i++) {
                    ISElement<?> te = (ISElement<?>) x1.get(i);
                    ISElement<?> oe = (ISElement<?>) y1.get(i);
                    SToolInterface s = manager.getSTool(te.getEquivSpec());
                    sim = s.similarity(te, oe) + sim;
                }
                return sim;
            } else return 0.0;
        }

        protected double similarity(ISElement<?> x1, ISElement<?> y1) {
            SToolInterface s = manager.getSTool(x1.getEquivSpec());
            if (s==null) {
                System.err.println(String.format("SELement %s equivspec %s",x1,x1.getEquivSpec()));
            }
            return s.similarity(x1, y1);
        }


    @Override
    protected Boolean equals(SElementTuple<?> x1, SElementTuple<?> y1) {
        return (x1.equals(y1));
    }
}
