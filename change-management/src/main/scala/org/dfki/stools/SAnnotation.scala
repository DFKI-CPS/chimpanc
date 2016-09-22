package org.dfki.stools

import scala.beans.BeanProperty

sealed trait SAnnotationOrConflict[T]

trait SAnnotation[T] extends SAnnotationOrConflict[T] {
  def getObject(): T
  def getName(): String
  def getValue(): String
  def getNameSpace(): String
  def getParent(): ISElement[_]
}

case class SAnnotationConflict[T](
    @BeanProperty `type`: SConflictTypes,
    @BeanProperty left: java.util.List[SAnnotation[_]],
    @BeanProperty right: java.util.List[SAnnotation[_]]) extends SAnnotationOrConflict[T] {
  override def toString() = {
    val form = 
      if (getType == SConflictTypes.list) "C(%s|%s)"
      else if (getType == SConflictTypes.updatedelete) "C<%s|%s>"
      else "C{%s|%s}"
    String.format(form,getLeft,getRight)
  }
}