package specific.secore

import org.dfki.stools.ISElement
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.ecore.{EPackage, EObject}
import org.dfki.stools.SAnnotation
import org.dfki.stools.similarityspec.ElementSimilaritySpec
import org.eclipse.ocl.ecore.Constraint
import specific.Config
import specific.graph.GraphEObject
import scala.collection.JavaConversions._
import org.dfki.stools.SElement
import scala.beans.BeanProperty

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
trait SEObject { self: SEStructuralFeature with SResource => 
  case class SEObject(val underlying: EObject, host: String) extends SElement[EObject] with ISElement[EObject] {
    def getObject(): EObject = underlying

    lazy val getChildren: java.util.List[ISElement[_]] =
      underlying.eContents().filter(Config.filter).map(SEObject(_, host))
  
    lazy val getParent: ISElement[_] = 
      Option(underlying.eContainer()).map(SEObject(_,host)).getOrElse(new SResource(underlying.eResource(), host))
  
    def getType(): String =
      underlying.eClass().getName()
  
    def getNamespace(): String = ""

    def getLabel(): String = getType

    def getURI(local: Resource, host: String, obj: EObject): URI = {
      if (obj.eResource() == local) {
        val oldUri = EcoreUtil.getURI(obj)
        val newUri = URI.createURI(s"graph://$host#${oldUri.fragment()}")
        newUri
      } else EcoreUtil.getURI(obj)
    }

    lazy val getAnnotations: java.util.List[SAnnotation[_]] =
      (underlying.eClass().getEAllStructuralFeatures())
        .filter(f => !f.isDerived && underlying.eIsSet(f))
        .map(a => new SEStructuralFeature(a, this, host)) ++ (underlying.eClass().getName match {
        case "Constraint" =>
          val repr = underlying match {
            case g: GraphEObject => g.plain("representation").get
            case other => other.toString
          }
          Some(ConstAnnotation("representation", repr, this))
        case _ => None
      })
  
    def hasAnnotation(namespace: String, name: String): java.lang.Boolean = {
      getAnnotations.exists(a => a.getNameSpace() == namespace && a.getName() == name)
    }
      
    def getAnnotation(namespace: String, name: String): SAnnotation[_] =
      getAnnotations.find(a => a.getNameSpace() == namespace && a.getName() == name).getOrElse(null)
  
    @BeanProperty
    var equivSpec: String = null
  
    @BeanProperty
    var similaritySpec: ElementSimilaritySpec = null
    
    def copy(): SElement[EObject] = {
      val result = new SEObject(underlying, host)
      result.equivSpec = this.equivSpec
      result.similaritySpec = this.similaritySpec
      result.setEditScript(getEditScript())
      result
    }
    
    override def equals(that: Any) = that match {
      case that: SEObject => that.underlying == this.underlying
      case _ => false
    }
  
    override def toString = {
      getLabel
    }
  }
}