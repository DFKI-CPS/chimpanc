package org.dfki.stools.editscript

import org.dfki.stools.SAnnotationOrConflict
import org.dfki.stools.ISElement
import scala.beans.BeanProperty
import scala.annotation.meta.getter
import org.dfki.stools.SElement
import scala.collection.JavaConversions._
import org.dfki.stools.SElementOrConflict
import org.dfki.stools.SAnnotation
import org.dfki.stools.SAnnotationConflict

trait SEdit

/*
 * @BeanProperty annotations are for Java Interop
 */

case class AppendAnnotations(
    @BeanProperty el: ISElement[_],
    @BeanProperty annotations: java.util.List[SAnnotationOrConflict[_]]) extends SEdit {  
  override def toString = s"appendAnnotations($getAnnotations)"
  def +(other: AppendAnnotations) = {
    assert(el == other.el)
    AppendAnnotations(el,annotations ++ other.annotations)
  }
}

case class AppendElements(
    @BeanProperty el: ISElement[_],
    @BeanProperty elements: java.util.List[SElementOrConflict[_]]) extends SEdit {  
  override def toString = elements.map {
    case e: SElement[_] if !e.getEditScript.isEmpty => e.toString() + e.getEditScript
    case e => e.toString()
  }.mkString ("append(",",",")")  
}

case class InsertAfter(
    @BeanProperty ref: ISElement[_],
    @BeanProperty elements: java.util.List[SElementOrConflict[_]]) extends SEdit {
  override def toString = s"insert-after($ref,$elements)"
}

case class InsertBefore(
    @BeanProperty ref: ISElement[_],
    @BeanProperty elements: java.util.List[SElementOrConflict[_]]) extends SEdit {
  override def toString = s"insert-before($ref,$elements)"
}
    
case class RemoveAnnotations(
    @BeanProperty ref: ISElement[_],
    @BeanProperty a: java.util.List[SAnnotation[_]]) extends SEdit {
  override def toString = s"removeAnnotations($a)"
}

case class RemoveElements(
    @BeanProperty ref: ISElement[_],
    @BeanProperty el: java.util.List[ISElement[_]]) extends SEdit {
  override def toString = s"remove($el)"
}

case class ReplaceElement(
    @BeanProperty ref: ISElement[_],
    @BeanProperty oldElement: ISElement[_],
    @BeanProperty newElement: SElementOrConflict[_]) extends SEdit {
  override def toString = s"replace($oldElement --> $newElement)"
}

case class UpdateAnnotation(
    @BeanProperty ref: ISElement[_],
    @BeanProperty oldAnnotation: SAnnotation[_],
    @BeanProperty newAnnotation: SAnnotationOrConflict[_]) extends SEdit {
  override def toString = {
    val name = oldAnnotation.getName()
    val o = oldAnnotation.getValue()
    val n = newAnnotation match {
      case a: SAnnotation[_] => a.getValue()
      case c: SAnnotationConflict[_] => c.toString()
    }
    s"$name[$o|-->$n]"
  }
}
