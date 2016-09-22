package specific.nl

import org.dfki.stools.similarityspec.ElementSimilaritySpec
import org.dfki.stools.{SAnnotation, ISElement, SElement}
import org.eclipse.emf.ecore.resource.Resource
import specific.Requirement
import scala.collection.JavaConversions._
import scala.beans.BeanProperty

/**
 * Created by martin on 11.09.15.
 */
/*class SRequirements(layer: String, underlying: Seq[Requirement]) extends SElement[Seq[Requirement]] with ISElement[Seq[Requirement]] {
  def getObject(): Seq[Requirement] = underlying

  lazy val getChildren: java.util.List[ISElement[_]] =
    underlying.map(_ => null)

  val getParent: ISElement[_] = null

  def getType(): String = "<root>"

  def getNamespace(): String = ""

  def getLabel(): String = "requirements"

  def getAnnotations(): java.util.List[SAnnotation[_]] = List.empty[SAnnotation[_]]

  def hasAnnotation(namespace: String, name: String): java.lang.Boolean =
    false

  def getAnnotation(namespace: String, name: String): SAnnotation[_] =
    null

  @BeanProperty
  var equivSpec: String = null

  @BeanProperty
  var similaritySpec: ElementSimilaritySpec = null

  def copy(): SElement[Seq[Requirement]] = {
    val result = new SRequirements(layer, underlying)
    result.setEquivSpec(getEquivSpec())
    result.setSimilaritySpec(getSimilaritySpec())
    result.setEditScript(getEditScript())
    result
  }

  override def toString = {
    layer
  }
}

class SRequirement(parent: SRequirements, underlying: Requirement) extends SElement[Requirement] with ISElement[Requirement] {
  def getObject(): Requirement = underlying

  lazy val getChildren: java.util.List[ISElement[_]] = List.empty

  val getParent: ISElement[_] = null

  def getType(): String = "<root>"

  def getNamespace(): String = ""

  def getLabel(): String = "requirement"

  def getAnnotations(): java.util.List[SAnnotation[_]] =
    List(SAnnotation underlying.name)

  def hasAnnotation(namespace: String, name: String): java.lang.Boolean =
    false

  def getAnnotation(namespace: String, name: String): SAnnotation[_] =
    null

  @BeanProperty
  var equivSpec: String = null

  @BeanProperty
  var similaritySpec: ElementSimilaritySpec = null

  def copy(): SElement[Seq[Requirement]] = {
    val result = new SRequirements(layer, underlying)
    result.setEquivSpec(getEquivSpec())
    result.setSimilaritySpec(getSimilaritySpec())
    result.setEditScript(getEditScript())
    result
  }

  override def toString = {
    layer
  }
}
*/