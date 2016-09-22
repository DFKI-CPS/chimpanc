package org.dfki.stools

import java.util.ArrayList
import java.util.List
import scala.beans.BeanProperty
import org.dfki.stools.similarityspec.ElementSimilaritySpec

class SAtomicString(
    line: String, 
    @BeanProperty var equivSpec: String, 
    @BeanProperty val parent: ISElement[_]) extends SElement[String] {      
  override def copy() = {
    val c = new SAtomicString("",equivSpec,null)
    c.setSimilaritySpec(similaritySpec)
    c;
  }
  
  override def getObject() = line

  override def toString() = s"[AString '$line']"
  
  override def getChildren() = new ArrayList[ISElement[_]]

  override def getType() = "<TEXT>"

  override def getNamespace() = ""

  override def getLabel() = line

  override def getAnnotations = new ArrayList[SAnnotation[_]]

  override def hasAnnotation(namespace: String, name: String) = false

  override def getAnnotation(namespace: String, name: String) = null
 
  @BeanProperty
  var similaritySpec: ElementSimilaritySpec = null
}
