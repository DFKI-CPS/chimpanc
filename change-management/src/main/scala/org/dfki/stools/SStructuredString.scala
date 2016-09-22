package org.dfki.stools

import java.util.ArrayList
import java.util.List
import scala.beans.BeanProperty
import scala.collection.JavaConversions._
import org.dfki.stools.similarityspec.ElementSimilaritySpec

class SStructuredString(
    line: String, 
    @BeanProperty var equivSpec: String,
    @BeanProperty val parent: ISElement[String]) extends SElement[String] {        
  override def copy() = {
    val c = new SStructuredString("",equivSpec,null)        
    c.setSimilaritySpec(similaritySpec)
    c
  }

  override def getObject() = line

  override def toString() = s"[SString '$line']"
  
  @BeanProperty lazy val children: java.util.List[ISElement[_]] = {
    val cs: Array[ISElement[_]] = equivSpec match {  
      case "wordlist" | "wordset" =>
        line.split(" ").flatMap(Array(_," ")).init.map(new SAtomicString(_,"word",this))
      case "charsetlist" | "charsetset" =>
        line.split(" ").flatMap(Array(_," ")).init.map(new SStructuredString(_, "charset", this))
      case "charlistlist" | "charlistset" =>
        line.split(" ").flatMap(Array(_," ")).init.map(new SStructuredString(_, "charlist", this))
      case "charset" | "charlist" =>
        line.toCharArray().map(c => new SAtomicString(c.toString, "char", this))
      case _ =>              
        System.err.println(s"Nothing for $this and $equivSpec");
        Array()
    }
    cs.foreach(_.setEquivSpec(equivSpec))
    cs.toList
  }

  override def getType = "<TEXT>"

  override def getNamespace = ""

  override def getLabel = getType

  override def getAnnotations = new ArrayList[SAnnotation[_]]
  
  override def hasAnnotation(namespace: String, name: String) = false

  override def getAnnotation(namespace: String, name: String) = null
 
  @BeanProperty
  var similaritySpec: ElementSimilaritySpec = null
}
