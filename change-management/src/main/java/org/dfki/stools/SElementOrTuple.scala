/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2011.
 */
package org.dfki.stools

import java.util.ArrayList

import org.dfki.stools.similarityspec.ElementNameSpec

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: 11.02.2011
 * Time: 17:00
 * To change this template use File | Settings | File Templates.
 */
trait SElementOrTuple[T]

class SElementTuple[T] extends ArrayList[ISElement[_]] with SElementOrTuple[T] {
  def contains(ens: ElementNameSpec): Boolean = {
    import scala.collection.JavaConversions._
    for (e <- this) {
      if ((e.getNamespace == ens.getNamespace) && (e.getType == ens.getName)) return true
    }
    false
  }

  def compatible(o: SElementTuple[_]): Boolean = {
    if (o.size == size) {
      {
        var i: Int = 0
        while (i < size) {
          {
            val te: ISElement[_] = get(i)
            val oe: ISElement[_] = o.get(i)
            if (!((te.getNamespace == oe.getNamespace) && (te.getType == oe.getType))) return false
          }
          i += 1; i - 1
        }
      }
      true
    }
    else false
  }

  def equals(o: SElementTuple[_]): Boolean = {
    if (o.size == size) {
      {
        var i: Int = 0
        while (i < size) {
          {
            val te: ISElement[_] = get(i)
            val oe: ISElement[_] = o.get(i)
            if (!(te == oe)) return false
          }
          i += 1; i - 1
        }
      }
      true
    }
    else false
  }
}