package specific.secore

import de.dfki.cps.stools.editscript.SEditScript
import specific.graph._
import de.dfki.cps.stools._
import scala.collection.JavaConversions._
import org.eclipse.emf.ecore.EObject

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
object SEcore
  extends SResource with SEObject with SEStructuralFeature {

  def applyDiff(diff: SEditScript) {
    println(diff)
    diff.entries.foreach {
    case (k,v) =>
      k.getObject match {
        case r: GraphResource =>
          v.appendAnnotations.foreach { append => println("unhandeled append annotations " + append) }
          v.removeAnnotations.foreach { remove => println("unhandeled remove annotations " + remove) }
          v.updateAnnotations.foreach { update => println("unhandeled update annotations " + update) }
          v.appendElements.foreach { append =>
            val el = append.getEl()
            val els = append.getElements()
            el.getObject() match {
              case g: GraphResource =>
                els.foreach {
                  case s: SElement[EObject] =>
                    println("append elements: " + s.getObject())
                    require(g.append(s.getObject()))
                  case c: SElementConflict[_] => sys.error("cannot handle conflicts")
                }
            }
          }
          v.insertAfter.foreach { case (el,insert) =>
            val el = insert.getRef().getObject().asInstanceOf[GraphEObject]
            insert.getElements().reverse.foreach {
              case s: SElement[EObject] =>
                println("insert after: " + s.getObject())
                require(el.insertAfter(s.getObject()))
              case c: SElementConflict[_] => sys.error("cannot handle conflicts")
            }
          }
          v.insertBefore.foreach { case (_, insert) =>
            val el = insert.getRef().getObject().asInstanceOf[GraphEObject]
            insert.getElements().foreach {
              case s: SElement[EObject] =>
                println("insert before: " + s.getObject())
                require(el.insertBefore(s.getObject()))
              case c: SElementConflict[_] => sys.error("cannot handle conflicts")
            }
          }
          v.removeElements.foreach { remove =>
            println("remove elements:  " + remove.el)
            remove.el.foreach(e => e.getObject().asInstanceOf[GraphEObject].remove())
          }
          v.replaceElements.foreach { replace =>
            println("replace elements")
            val el = replace.getOldElement()
            val el2 = replace.getNewElement() match {
              case s: SElement[EObject] => s.getObject()
              case c: SElementConflict[_] => sys.error("cannot handle conflicts")
            }
            val old = el.getObject().asInstanceOf[GraphEObject]
            require(old.insertAfter(el2))
            require(old.remove())
          }
        case o: GraphEObject =>
          println(diff.entries)
          v.appendAnnotations.foreach { append =>
            append.annotations.foreach { 
              case a: SAnnotation[_] =>
                println("append annotation: " + a.getName() + " -> " + a.getValue())
                o.setPlain(a.getName(),a.getValue())
              case c: SAnnotationConflict[_] =>
                sys.error("conflict resolution is not implemented yet")
            }
          }
          v.appendElements.foreach { append =>            
            val el = append.getEl()
            val els = append.getElements()
            el.getObject() match {
              case g: GraphEObject =>
                els.foreach {
                  case s: SElement[EObject] =>
                    println("append elements: " + s.getObject())
                    require(g.append(s.getObject()))
                  case c: SElementConflict[_] => sys.error("cannot handle conflicts")
                }
            }            
          }
          v.insertAfter.foreach { case (el,insert) =>
            val el = insert.getRef().getObject().asInstanceOf[GraphEObject]
            insert.getElements().reverse.foreach {
              case s: SElement[EObject] =>
                println("insert after: " + s.getObject())
                require(el.insertAfter(s.getObject()))
              case c: SElementConflict[_] => sys.error("cannot handle conflicts")
            }
          }
          v.insertBefore.foreach { case (_, insert) =>
            val el = insert.getRef().getObject().asInstanceOf[GraphEObject]
            insert.getElements().foreach {
              case s: SElement[EObject] =>
                println("insert before: " + s.getObject())
                require(el.insertBefore(s.getObject()))
              case c: SElementConflict[_] => sys.error("cannot handle conflicts")
            }
          }
          v.removeAnnotations.foreach { remove =>
            remove.a.foreach { a =>
              println("remove annotation: " + a.getName())
              val feature = o.eClass().getEStructuralFeature(a.getName())
              o.eUnset(feature)
            }
          }
          v.removeElements.foreach { remove =>
            println("remove elements:  " + remove.el)
            remove.el.foreach(e => e.getObject().asInstanceOf[GraphEObject].remove())
          }
          v.replaceElements.foreach { replace =>
            println("replace elements")
            val el = replace.getOldElement()
            val el2 = replace.getNewElement() match {
              case s: SElement[EObject] => s.getObject()
              case c: SElementConflict[_] => sys.error("cannot handle conflicts")
            }
            val old = el.getObject().asInstanceOf[GraphEObject]
            require(old.insertAfter(el2))
            require(old.remove())
          }          
          v.updateAnnotations.foreach { update =>
            println("update annotations")
            update.newAnnotation match {
              case a: SAnnotation[_] =>
                require(o.setPlain(a.getName(),a.getValue()))
              case c: SAnnotationConflict[_] =>
                sys.error("conflict resolution is not implemented yet")
            }
          }
        case other => println(s"warn: unhandled diff on $other")
      }      
    }  
  }
}