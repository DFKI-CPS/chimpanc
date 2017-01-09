package specific.secore

import de.dfki.cps.stools.SAnnotation
import de.dfki.cps.stools.ISElement
import org.eclipse.emf.common.util.{EList, URI}
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.xmi.XMLResource
import org.eclipse.emf.ecore.util.EcoreUtil
import specific.graph.GraphResource
import specific.graph.GraphEObject
import scala.collection.JavaConversions._

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
trait SEStructuralFeature { self: SEObject => 
  class SEStructuralFeature(underlying: EStructuralFeature, ref: SEObject, host: String) extends SAnnotation[EStructuralFeature] {
    def getObject(): EStructuralFeature = underlying
    
    def getName(): String = underlying.getName()
    
    def getNameSpace(): String = ""
    
    def getParent(): ISElement[_] = ref

    def getURI(local: Resource, host: String, obj: EObject): URI = {
      if (obj.eResource() == local) {
        val oldUri = EcoreUtil.getURI(obj)
        val newUri = URI.createURI(s"graph://$host#${oldUri.fragment()}")
        newUri
      } else EcoreUtil.getURI(obj)
    }

    def getValue(): String = ref.getObject() match {
      case g: GraphEObject =>
        g.plain(underlying.getName()).getOrElse("null")
      case e: EObject => e.eGet(underlying) match {
        case value: EObject =>
          getURI(ref.getObject().eResource(), host, value).toString
        case values: EList[_] =>
          values.collect { // TODO: handle strings
            case value: EObject =>
              getURI(ref.getObject().eResource(), host, value).toString
          }.mkString(",")
        case other =>
          other.toString
      }
    }
    
    override def toString = s"$getNameSpace:$getName: $getValue"
  }
}

case class ConstAnnotation[T](key: String, value: T, parent: ISElement[_]) extends SAnnotation[T] {
  def getObject() = value
  def getName() = key
  def getNameSpace() = ""
  def getParent() = parent
  def getValue() = value.toString
}