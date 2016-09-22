package specific.secore

import org.dfki.stools.SElement
import org.eclipse.emf.ecore.EPackage
import org.eclipse.emf.ecore.resource.Resource
import org.dfki.stools.ISElement
import specific.Config
import scala.collection.JavaConversions._
import org.dfki.stools.SAnnotation
import scala.beans.BeanProperty
import org.dfki.stools.similarityspec.ElementSimilaritySpec

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
trait SResource { self: SEObject =>
  class SResource(underlying: Resource, host: String) extends SElement[Resource] with ISElement[Resource] {
    def getObject(): Resource = underlying
      
    lazy val getChildren: java.util.List[ISElement[_]] =
      underlying.getContents().collect {
        case p: EPackage if !Config.ignorePackages.contains(p.getName) => p
        case x if !x.isInstanceOf[EPackage] => x
      }.map(new SEObject(_,host))
      
    val getParent: ISElement[_] = null
      
    def getType(): String =
      "root"
    
    def getNamespace(): String =
      ""
      
    def getLabel(): String = getType
  
    def getAnnotations(): java.util.List[SAnnotation[_]] =
      List.empty[SAnnotation[_]]      
      
    def hasAnnotation(namespace: String, name: String): java.lang.Boolean = 
      false    
      
    def getAnnotation(namespace: String, name: String): SAnnotation[_] =
      null
    
    @BeanProperty
    var equivSpec: String = null
    
    @BeanProperty
    var similaritySpec: ElementSimilaritySpec = null    
    
    def copy(): SElement[Resource] = {
      val result = new SResource(underlying, host)
      result.setEquivSpec(getEquivSpec())
      result.setSimilaritySpec(getSimilaritySpec())
      result.setEditScript(getEditScript())
      result
    }
    
    override def toString = {
      getLabel
    }  
  }
}