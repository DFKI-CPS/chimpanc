package org.dfki.stools

import org.dfki.stools.similarityspec.ElementSimilaritySpec
import org.dfki.stools.editscript.SEditScript
import org.dfki.stools.similarityspec.ElementNameSpec
import scala.collection.JavaConversions._
import scala.beans.BeanProperty

sealed trait SElementOrConflict[T]

trait ISElement[T] extends SElementOrTuple[T] with SElementOrConflict[T] {
  def getObject(): T
  def getChildren(): java.util.List[ISElement[_]]
  def getType(): String
  def getNamespace(): String
  def getLabel(): String
  def getAnnotations(): java.util.List[SAnnotation[_]]
  def hasAnnotation(namespace: String, name: String): java.lang.Boolean
  def getAnnotation(namespace: String, name: String): SAnnotation[_]
  def setEquivSpec(name: String): Unit
  def getEquivSpec(): String
  def setSimilaritySpec(s: ElementSimilaritySpec)
  def getSimilaritySpec(): ElementSimilaritySpec
  def getParent(): ISElement[_]
  def copy(): ISElement[_]
  def setEditScript(r: SEditScript)
  def getEditScript(): SEditScript
}

abstract class SElement[T] extends ISElement[T] {
  protected var patch = new SEditScript

  override def setEditScript(r: SEditScript) = { 
    if (!patch.isEmpty) patch = new SEditScript()
    patch.insertAll(r)
  }

  override def getEditScript() = patch
}

case class SElementConflict[T](
    @BeanProperty `type`: SConflictTypes,
    @BeanProperty left: java.util.List[ISElement[_]],
    @BeanProperty right: java.util.List[ISElement[_]]) extends SElementOrConflict[T] {
  override def toString = {
    val form =
      if (getType == SConflictTypes.list) "C(%s|%s)"
      else if (getType == SConflictTypes.updatedelete) "C<%s|%s>" 
      else "C{%s|%s}"
    String.format(form,getLeft,getRight);
  }
}

/*class SElementTuple[T] extends java.util.ArrayList[ISElement[_]] with SElementOrTuple[T] {
  def contains(ens: ElementNameSpec) = 
    this.exists { e => e.getNamespace() == ens.getNamespace && e.getType == ens.getName }
  
  def compatible(o: SElementTuple[_]) =
    (o.size == this.size) && 
    (this.zip(o).forall { case (a,b) => a.getNamespace == b.getNamespace && a.getType == b.getType })

  def equals(o: SElementTuple[_]) =
    (o.size == this.size) &&
    (this.zip(o).forall { case (a,b) => a == b })
}*/