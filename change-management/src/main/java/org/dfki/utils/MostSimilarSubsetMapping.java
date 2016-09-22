/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2010.
 */

package org.dfki.utils;

import java.util.*;

import scala.Tuple2;
import scala.Tuple3;
import static java.lang.Math.abs;

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: Nov 3, 2010
 * Time: 9:52:03 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class MostSimilarSubsetMapping<VALUE> {

    SortedMatrixEntries c;
    List<MatrixEntry> fixed;
    Map<Integer,Integer> marked;
    ArrayList<Integer> disabledX;
    ArrayList<Integer> disabledY;
    HashMap<Integer, Integer> mapping;
    Double similarity;
    int size;

    ArrayList<VALUE> xSubset;
    ArrayList<VALUE> ySubset;
    ArrayList<MostSimilarSubsequenceMapping.DiffEntry<VALUE>> diff;

    /* abstract setter methods */
    public abstract void setX(List<VALUE> xl);

    public abstract void setY(List<VALUE> yl);

    public abstract void initFrom(MostSimilarSubsetMapping<VALUE> other);

    /* getter methods */

    public List<VALUE> getX() {
        List<VALUE> r = new ArrayList<VALUE>(sizeOfX());
        for (int i = 0; i < sizeOfX(); i++) r.add(valueOfX(i));
        return r;
    }

    public List<VALUE> getY() {
        List<VALUE> r = new ArrayList<VALUE>(sizeOfY());
        for (int i = 0; i < sizeOfY(); i++) r.add(valueOfY(i));
        return r;
    }

    protected abstract int sizeOfX();

    protected abstract int sizeOfY();

    protected abstract VALUE valueOfX(int index);

    protected abstract VALUE valueOfY(int index);

    protected abstract double similarity(VALUE x1, VALUE y1);

    protected abstract Boolean equals(VALUE x1, VALUE y1);

    public Boolean fixMapping(Map<Integer, Integer> mapXY) {
        marked = new HashMap<Integer, Integer>();
        for (Integer xpos : mapXY.keySet()) {
            marked.put(xpos, mapXY.get(xpos));
        }
        //System.out.println("Marked: "+marked);
        return true;
    }

    public void calculateMSM() {
        if (c != null) {
            return;
        } else {
            c = new SortedMatrixEntries();
            fixed = new ArrayList<MatrixEntry>();
            if (marked==null) marked = new HashMap<Integer, Integer>();

            for (int i = 0; i < sizeOfX(); i++) {
                for (int j = 0; j < sizeOfY(); j++) {
                    MatrixEntry m = new MatrixEntry(i, j, similarity(valueOfX(i), valueOfY(j)));
                    if (marked.containsKey(i) && marked.get(i)==j) fixed.add(m);
                    else c.insert(m);
                }
            }
            Collections.reverse(c);
            mapping = new HashMap<Integer, Integer>();
            disabledX = new ArrayList<Integer>();
            disabledY = new ArrayList<Integer>();
            size = 0;
            similarity = 0.0;

            // First taking those that are fixed;

            for (MatrixEntry m : fixed) {
                this.mapping.put(m.xIndex, m.yIndex);
                //this.xSubset.add(valueOfX(m.xIndex));
                //this.ySubset.add(valueOfY(m.yIndex));
                this.size = this.size + 1;
                this.similarity = this.similarity + m.similarity;
                disabledX.add(m.xIndex);
                disabledY.add(m.yIndex);
            }

            // Then take the rest.

            for (MatrixEntry m : c) {
                if (m.similarity > 0) {
                    if (!disabledX.contains(m.xIndex) &&
                            !disabledY.contains(m.yIndex)) {
                        this.mapping.put(m.xIndex, m.yIndex);
                        //this.xSubset.add(valueOfX(m.xIndex));
                        //this.ySubset.add(valueOfY(m.yIndex));
                        this.size = this.size + 1;
                        this.similarity = this.similarity + m.similarity;
                        disabledX.add(m.xIndex);
                        disabledY.add(m.yIndex);
                    }
                } else break;
            }
            if (sizeOfX()==0 && sizeOfY()==0) similarity = 1.0;

            Map<Integer,VALUE> xSubsetMap = new HashMap<Integer,VALUE>(size);
            Map<Integer,VALUE> ySubsetMap = new HashMap<Integer,VALUE>(size);

            this.xSubset = new ArrayList<VALUE>(size);
            this.ySubset = new ArrayList<VALUE>(size);

            diff = new ArrayList<MostSimilarSubsequenceMapping.DiffEntry<VALUE>>();

            for (int i = 0; i < sizeOfX(); i++) {
                if (mapping.containsKey(i)) {
                    xSubset.add(valueOfX(i));
                    ySubset.add(valueOfY(mapping.get(i)));
                    if (equals(valueOfX(i), valueOfY(mapping.get(i)))) {
                        diff.add(new MostSimilarSubsequenceMapping.DiffEntry<VALUE>(
                                MostSimilarSubsequenceMapping.DiffType.NONE, valueOfX(i)));
                    } else {
                        diff.add(new MostSimilarSubsequenceMapping.DiffEntry<VALUE>(
                                MostSimilarSubsequenceMapping.DiffType.UPDATE, valueOfY(mapping.get(i))));
                    }
                } else {
                    diff.add(new MostSimilarSubsequenceMapping.DiffEntry<VALUE>(
                            MostSimilarSubsequenceMapping.DiffType.REMOVE, valueOfX(i)));
                }
            }
            for (int j = 0; j < sizeOfY(); j++) {
                if (!mapping.values().contains(j)) {
                    diff.add(new MostSimilarSubsequenceMapping.DiffEntry<VALUE>(
                            MostSimilarSubsequenceMapping.DiffType.ADD, valueOfY(j)));
                }
            }



        }
    }

    public Double getMSMSimilarity() {
        calculateMSM();
        return this.similarity;
    }

    public int getMSMSize() {
        calculateMSM();
        return this.size;
    }


    public int getMinEditDistance() {
        calculateMSM();
        return sizeOfX() + sizeOfY() - 2 * abs(getMSMSize());
    }

    public List<VALUE> getSubsetX() {
        calculateMSM();
        return xSubset;
    }

    public List<VALUE> getSubsetY() {
        calculateMSM();
        return ySubset;
    }

    public List<MostSimilarSubsequenceMapping.DiffEntry<VALUE>> getDiff() {
        calculateMSM();
        return diff;
    }
    public List<Map.Entry<Integer, Integer>> getOrderedMapping() {
        calculateMSM();
        List<Map.Entry<Integer, Integer>> result = new ArrayList<Map.Entry<Integer, Integer>>(mapping.size());
        result.addAll(mapping.entrySet());
        Collections.sort(result, new Comparator<Map.Entry<Integer, Integer>>() {
            @Override
            public int compare(Map.Entry<Integer, Integer> e1,
                               Map.Entry<Integer, Integer> e2) {
                if (e1.getKey() > e2.getKey()) return 1;
                else if (e1.getKey() < e2.getKey()) return -1;
                else return 0;
            }
        });
        return result;
    }

    public List<MostSimilarSubsequenceMapping.DiffEntry<VALUE>> getDiff3(VALUE[] origin) {
        List<MostSimilarSubsequenceMapping.DiffEntry<VALUE>> result = null;
        try {
            Class c = this.getClass();
            MostSimilarSubsetMapping<VALUE> MSSMOrigin2commonXY = (MostSimilarSubsetMapping<VALUE>) c.newInstance();
            MSSMOrigin2commonXY.initFrom(this);
            MostSimilarSubsetMapping<VALUE> MSSMOrigin2X = (MostSimilarSubsetMapping<VALUE>) c.newInstance();
            MSSMOrigin2X.initFrom(this);
            MostSimilarSubsetMapping<VALUE> MSSMOrigin2Y = (MostSimilarSubsetMapping<VALUE>) c.newInstance();
            MSSMOrigin2Y.initFrom(this);

            // 1. Determine longest common subsequence between X and Y
            //System.out.println("Class = "+this.getClass());
            //System.out.println("X = "+getX());
            //System.out.println("Y = "+getY());

            List<VALUE> commonXY = getSubsetX();
            //System.out.println("CommonXY = "+commonXY);

            // 2. Determine LCS between origin and commonXY
            MSSMOrigin2commonXY.setX(Arrays.asList(origin));
            MSSMOrigin2commonXY.setY(commonXY);
            MSSMOrigin2commonXY.calculateMSM();
            // 3. Compute mappings for marking object between Origin and X as well as Origin and Y
            //System.out.println("mapAB = "+this.mapping);
            //System.out.println("mapOrigin2CommonAB = "+MSSMOrigin2commonXY.mapping);
            Tuple2<Map<Integer, Integer>, Map<Integer, Integer>> maps =
                    MostSimilarSubsequenceMapping.computeMappingsFromCommonMapping(MSSMOrigin2commonXY.mapping, this.getOrderedMapping());
            //System.out.println("O->X: "+maps._1());
            //System.out.println("O->Y: " + maps._2());

            // 4. Compute LCS(O,X) containing LCS(O,LCS(XY));
            List<VALUE> X = getX();
            MSSMOrigin2X.setX(Arrays.asList(origin));
            MSSMOrigin2X.setY(X);
            MSSMOrigin2X.fixMapping(maps._1());
            MSSMOrigin2X.calculateMSM();

            // 5. Compute LCS(O,Y) containing LCS(O,LCS(XY));
            List<VALUE> Y = getY();
            MSSMOrigin2Y.setX(Arrays.asList(origin));
            MSSMOrigin2Y.setY(Y);
            MSSMOrigin2Y.fixMapping(maps._2());
            MSSMOrigin2Y.calculateMSM();

            // 6. Compute the alignment between O, X and Y using the mappings Origin2X, Origin2Y and X2Y
            Alignment<VALUE> m = new Alignment<VALUE>(Arrays.asList(origin), X, Y, MSSMOrigin2X.mapping, MSSMOrigin2Y.mapping, this.mapping, this);
            m.compute();
            //System.out.println(m);
            //System.out.println("HERE");
            result = m.getDiff3();

        } catch (InstantiationException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (IllegalAccessException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return result;
    }
    @Override
    public String toString() {
        calculateMSM();

        StringBuffer buf = new StringBuffer();
        buf.append("   ");
        buf.append("X = ");
        for (int i = 0; i < sizeOfX(); i++) buf.append(valueOfX(i) + " ");
        buf.append("\nY = ");
        for (int i = 0; i < sizeOfY(); i++) buf.append(valueOfY(i) + " ");
        buf.append("\n");
        buf.append("MSM(" + getMSMSize() + ") X = " + getSubsetX() + "\n");
        buf.append("MSM(" + getMSMSize() + ") Y = " + getSubsetY() + "\n");
        buf.append("Similarity = " + getMSMSimilarity()+"\n");
        buf.append("Diff: " + diff.toString() + "\n");
        buf.append("Min edit distance = " + getMinEditDistance() + "\n");

        return buf.toString();
    }

    public static class MatrixEntry implements Comparable {
        public Double similarity;
        public int xIndex;
        public int yIndex;

        public MatrixEntry(int x, int y, Double sim) {
            similarity = sim;
            xIndex = x;
            yIndex = y;
        }

        public String toString() {
            return "[" + xIndex + "][" + yIndex + "]=" + similarity;
        }

        public int compareTo(Object o) {
            if (o instanceof MatrixEntry) {
                MatrixEntry m = (MatrixEntry) o;
                if (similarity < m.similarity) return -1;
                else if (similarity.equals(m.similarity)) return 0;
                else return +1;
            } else return 0;
        }
    }

    public static class SortedMatrixEntries extends ArrayList<MatrixEntry> {

        public void insert(MatrixEntry m) {
            if (this.size() == 0) this.add(m);
            else if (this.get(0).similarity >= m.similarity) {
                this.add(0, m);
            } else if (this.get(this.size() - 1).similarity <= m.similarity) {
                this.add(m);
            } else insert(m, 0, this.size() - 1);
        }

        private void insert(MatrixEntry m, int left, int right) {
            if (left + 1 >= right) {
                this.add(right, m);
            } else {
                int mid = (left + right) / 2;
                MatrixEntry median = this.get(mid);
                if (median.similarity == m.similarity) {
                    this.add(mid, m);
                } else if (median.similarity > m.similarity) {
                    insert(m, left, mid);
                } else {
                    insert(m, mid, right);
                }
            }
        }
    }


    static class Alignment<T extends Object> {
        T[] O;
        T[] A;
        T[] B;
        Map<Integer, Integer> mapOA;
        Map<Integer, Integer> mapOB;
        Map<Integer, Integer> mapAB;
        List<T>[] alignment;
        List<MostSimilarSubsequenceMapping.DiffEntry<T>> diff;
        MostSimilarSubsetMapping<T> mssm;

        public Alignment(List<T> o, List<T> a, List<T> b, Map<Integer, Integer> oa, Map<Integer, Integer> ob, Map<Integer, Integer> ab,
                         MostSimilarSubsetMapping<T> mssm) {
            O = (T[]) o.toArray();
            A = (T[]) a.toArray();
            B = (T[]) b.toArray();
            mapOA = oa;
            mapOB = ob;
            mapAB = ab;
            alignment = new ArrayList[]{new ArrayList<T>(), new ArrayList<T>(), new ArrayList<T>()};
            diff = null;
            this.mssm = mssm;
        }

        private void add(Integer o, Integer a, Integer b) {
            //System.out.println(String.format("ADD: %s %s %s",o,a,b));
            alignment[0].add((o != null) ? O[o] : null);
            alignment[1].add((a != null) ? A[a] : null);
            alignment[2].add((b != null) ? B[b] : null);
        }

        private Tuple3<T, T, T> get(Integer i) {
            return new Tuple3<T, T, T>(alignment[0].get(i), alignment[1].get(i), alignment[2].get(i));
        }

        public void compute() {
            arrange();

            extractDiff();
        }

        public List<MostSimilarSubsequenceMapping.DiffEntry<T>> getDiff3() {
            if (diff == null) {
                compute();
            }
            return diff;
        }

        private void arrange() {
            Boolean stop = false;
            for(int opos=0;opos<O.length;opos++) {
                if (mapOA.containsKey(opos)) {
                    if (mapOB.containsKey(opos)) {
                        add(opos,mapOA.get(opos),mapOB.get(opos));
                    } else {
                        add(opos,mapOA.get(opos),null);
                    }
                } else if (mapOB.containsKey(opos)) {
                    add(opos,null,mapOB.get(opos));
                } else add(opos,null,null);
            }
            for(int apos=0;apos<A.length;apos++) {
                if (!mapOA.containsValue(apos)) {
                    if (mapAB.containsKey(apos)) {
                        add(null,apos,mapAB.get(apos));
                    } else {
                        add(null,apos,null);
                    }
                }
            }
            for(int bpos=0;bpos<B.length;bpos++) {
                if (!mapOB.containsValue(bpos) && !mapAB.containsValue(bpos)) {
                    add(null,null,bpos);
                }
            }

        }

        private Integer getOrigin(Map<Integer, Integer> m, Integer v) {
            Integer result = null;
            for (Integer key : m.keySet()) {
                if (m.get(key).equals(v)) result = key;
            }
            return result;
        }

        private void extractDiff() {
            diff = new ArrayList<MostSimilarSubsequenceMapping.DiffEntry<T>>();
            ArrayList<MostSimilarSubsequenceMapping.DiffEntry<T>> tempdiff = new ArrayList<MostSimilarSubsequenceMapping.DiffEntry<T>>();
            MostSimilarSubsequenceMapping.DiffEntry<T> last = null;
            for (int i = 0; i < alignment[0].size(); i++) {
                if (tempdiff.size() > 0) last = tempdiff.get(tempdiff.size() - 1);
                Tuple3<T, T, T> e = get(i);
                if (e._1() != null) {
                    if (e._2() == null || e._3() == null) {
                        if (e._2()!=null && !mssm.equals(e._1(),e._2())) {
                            // this is a conflict; one variant changed it, the other deleted it
                            tempdiff.add(new MostSimilarSubsequenceMapping.UpdateDeleteConflictEntry<T>(e._2(),null));
                        } else if (e._3()!=null && !mssm.equals(e._1(),e._3())) {
                            // this is a conflict; one variant changed it, the other deleted it
                            tempdiff.add(new MostSimilarSubsequenceMapping.UpdateDeleteConflictEntry<T>(null,e._3()));
                        } else
                            tempdiff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.REMOVE, e._1()));
                    }
                    else {
                        if (mssm.equals(e._2(), e._3())) {
                            if (mssm.equals(e._1(), e._2()))
                                tempdiff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.NONE, e._1()));
                            else
                                tempdiff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.UPDATE, e._2()));
                        } else if (mssm.equals(e._1(), e._2())) {
                            tempdiff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.UPDATE, e._3()));
                        } else tempdiff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.UPDATE, e._2(), e._3()));
                    }
                } else if (e._2() != null && e._3() != null) {
                    if (mssm.equals(e._2(), e._3()))
                        tempdiff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.ADD, e._2()));
                    else
                        tempdiff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.ADD, e._2(), e._3()));
                } else {
                    if (last instanceof MostSimilarSubsequenceMapping.ConflictEntry) {
                        MostSimilarSubsequenceMapping.SetConflictEntry<T> conflictEntry = (MostSimilarSubsequenceMapping.SetConflictEntry<T>) last;
                        if (e._2() != null) conflictEntry.addLeft(e._2());
                        else conflictEntry.addRight(e._3());
                    } else
                        tempdiff.add(new MostSimilarSubsequenceMapping.SetConflictEntry<T>(e._2(), e._3()));
                }
            }
            Iterator<MostSimilarSubsequenceMapping.DiffEntry<T>> it = tempdiff.iterator();
            while (it.hasNext()) {
                MostSimilarSubsequenceMapping.DiffEntry<T> e = it.next();
                if (e instanceof MostSimilarSubsequenceMapping.SetConflictEntry) {
                    MostSimilarSubsequenceMapping.SetConflictEntry<T> c = (MostSimilarSubsequenceMapping.SetConflictEntry<T>) e;
                    if (c.rightvalues.isEmpty()) for (T o : c.leftvalues) diff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.ADD, o));
                    else if (c.leftvalues.isEmpty())
                        for (T o : c.rightvalues) diff.add(new MostSimilarSubsequenceMapping.DiffEntry<T>(MostSimilarSubsequenceMapping.DiffType.ADD, o));
                    else diff.add(c);
                } else diff.add(e);
            }
        }

        private Boolean inMap(Map<Integer, Integer> m, Integer k, Integer v) {
            //System.out.println(String.format("INMAP: %s %s %s",m,k,v));
            return m.containsKey(k) && m.get(k).equals(v);
        }

        private String fillup(String s) {
            int size = 1;
            for (T[] X : (T[][]) new Object[][]{A, O, B}) {
                for (T e : X) {
                    size = Math.max(size, e.toString().length());
                }
            }

            String r = String.format("%1$#" + (size + 1) + "s", s);
            return r;
        }

        private void printList(List<T> al, StringBuffer bw) {
            for (int i = 0; i < al.size(); i++) {
                if (al.get(i) == null) bw.append(fillup("-"));
                else bw.append(fillup(al.get(i).toString()));
            }
        }

        private String printArray(T[] al) {
            StringBuffer bw = new StringBuffer();
            bw.append("[");
            for (int i = 0; i < al.length; i++) {
                if (al[i] == null) bw.append(fillup("-"));
                else bw.append(fillup(al[i].toString()));
            }
            bw.append("]");
            return bw.toString();
        }

        public String toString() {
            StringBuffer buf = new StringBuffer();
            buf.append("A = " + printArray(A) + "\n");
            buf.append("O = " + printArray(O) + "\n");
            buf.append("B = " + printArray(B) + "\n");
            buf.append("\nAlignment:\n----------------------------------\n");
            buf.append("A : ");
            printList(alignment[1], buf);
            buf.append("\n");
            buf.append("O : ");
            printList(alignment[0], buf);
            buf.append("\n");
            buf.append("B : ");
            printList(alignment[2], buf);
            buf.append("\n");
            return buf.toString();
        }
    }
}
