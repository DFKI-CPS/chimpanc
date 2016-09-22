package org.dfki.stools.editscript

import org.dfki.stools.ISElement
import scala.collection.mutable._

object SEditScript {
  class SEditScriptEntry {
    var updateAnnotations = Buffer.empty[UpdateAnnotation]
    var appendAnnotations = Option.empty[AppendAnnotations]
    var appendElements = Option.empty[AppendElements]
    var insertAfter = Map.empty[ISElement[_],InsertAfter]
    var insertBefore = Map.empty[ISElement[_],InsertBefore]
    var removeAnnotations = Option.empty[RemoveAnnotations]
    var removeElements = Option.empty[RemoveElements]
    var replaceElements = Buffer.empty[ReplaceElement]

    override def toString() =       
      List[TraversableOnce[Any]](updateAnnotations,appendAnnotations,
          appendElements,insertAfter,insertBefore,
          removeAnnotations,removeElements,replaceElements)
        .collect {
        case x if x.nonEmpty => x.mkString("[",", ","]")
      }.mkString(", ")          
  }
}

class SEditScript {
  val entries = collection.mutable.Map.empty[ISElement[_], SEditScript.SEditScriptEntry]
  
  def isEmpty = entries.isEmpty
  
  override def toString =
    entries.map { case (e,v) => s"*$e:\n  $v" }
           .mkString("{\n","\n","\n}")
  
  def getEntries(e: ISElement[_]) =
    entries.getOrElseUpdate(e, new SEditScript.SEditScriptEntry())
  
  def insertAll(es: SEditScript) {
    for ((e,entry) <- es.entries) {      
      entry.appendAnnotations.foreach(insert(e,_))
      entry.removeAnnotations.foreach(insert(e,_))      
      entry.updateAnnotations.foreach(insert(e,_))           
      entry.insertBefore.values.foreach(insert(e,_))
      entry.insertAfter.values.foreach(insert(e,_))
      entry.appendElements.foreach(insert(e,_))
      entry.removeElements.foreach(insert(e,_))
      entry.replaceElements.foreach(insert(e,_))
    }
  }

  def insert(e: ISElement[_], c: AppendAnnotations) = {    
    val entry = getEntries(e)
    if (entry.appendAnnotations == None) entry.appendAnnotations = Some(c)
    else entry.appendAnnotations.get.getAnnotations.addAll(c.getAnnotations)
  }

  def insert(e: ISElement[_], c: RemoveAnnotations) = {
    val entry = getEntries(e)
    if (entry.removeAnnotations == None) entry.removeAnnotations = Some(c)
    else entry.removeAnnotations.get.a.addAll(c.a)
  }
    
  def insert(e: ISElement[_], c: AppendElements) = {
    val entry = getEntries(e)
    if (entry.appendElements == None) entry.appendElements = Some(c)
    else entry.appendElements.get.elements.addAll(c.elements)
  }

  def insert(e: ISElement[_], c: RemoveElements) = {    
    val entry = getEntries(e);
    if (entry.removeElements == None) entry.removeElements = Some(c);
    else entry.removeElements.get.el.addAll(c.el);
    // Debug.println("Adding "+c);
  }

  def insert(e: ISElement[_], c: Buffer[ReplaceElement]) {
    val entry = getEntries(e);
    if (entry.replaceElements == null) entry.replaceElements = c;
    else entry.replaceElements.appendAll(c);
  }    

  def insert(e: ISElement[_], c: UpdateAnnotation) {
    val entry = getEntries(e);
    entry.updateAnnotations.append(c)
  }    
  
  def insert(e: ISElement[_], c: InsertAfter) {     
    val entry = getEntries(e);
    if (entry.insertAfter.contains(c.ref)) {
        entry.insertAfter(c.ref).elements.addAll(c.elements);
    } else {
        entry.insertAfter.put(c.ref, c);
    }
  }

  def insert(e: ISElement[_], c: InsertBefore) {      
    val entry = getEntries(e);
    if (entry.insertBefore.contains(c.ref)) {
        entry.insertBefore(c.ref).elements.addAll(c.elements);
    } else {
        entry.insertBefore.put(c.ref, c);
    }
    // Debug.println("Adding "+c);
  }

  def insert(e: ISElement[_], c: ReplaceElement) {
    val entry = getEntries(e);
    entry.replaceElements.append(c)
  }
}
