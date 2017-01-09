package specific.sysml

import org.eclipse.emf.ecore.{EAnnotation, EModelElement, EObject}
import org.eclipse.uml2.uml
import org.eclipse.emf.ecore.resource.Resource
import specific._

import scala.collection.JavaConversions._
import scala.util.Try

/**
  * Created by martin on 1/9/17.
  */
object SysML {
  def positioned(source: EObject, x: LayerObject): LayerObject = source match {
    case source: EModelElement if source.getEAnnotation("http://www.dfki.de/specific/SysML") != null =>
      val line = source.getEAnnotation("http://www.dfki.de/specific/SysML").getDetails.get("line").toInt
      val column = source.getEAnnotation("http://www.dfki.de/specific/SysML").getDetails.get("column").toInt
      PositionedLayerObject(x,line,column)
    case other => x
  }

  private def isIdChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  def getEntities(resource: Resource): Seq[LayerObject] = {
    resource.getAllContents.collect {
      case e: uml.Class if Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        positioned(e,Clazz(e.getName))
      case e: uml.StructuralFeature if Try(e.getFeaturingClassifiers.head.getName.forall(isIdChar)).toOption.contains(true)
        && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        positioned(e,Attribute(e.getFeaturingClassifiers.head.getName, e.getName, Option(e.getType).map(_.getName).getOrElse("void")))
      case e: uml.Operation if Try(e.getFeaturingClassifiers.head.getName.forall(isIdChar)).toOption.contains(true)
        && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        positioned(e,Operation(e.getFeaturingClassifiers.head.getName, e.getName, Option(e.getType).map(_.getName).getOrElse("void"), Try(e.getOwnedParameters.map(p => (p.getName, p.getType.getName))).getOrElse(Seq.empty)))
      case e: uml.Parameter if Try(e.getOperation.getFeaturingClassifiers.head.getName.forall(isIdChar)).toOption.contains(true)
        && Try(e.getOperation.getName.forall(isIdChar)).toOption.contains(true)
        && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        positioned(e,Parameter(Try(e.getOperation.getFeaturingClassifiers.head.getName).getOrElse("error"), Try(e.getOperation.getName).getOrElse("error"), Try(e.getName).getOrElse("error"), Try(e.getType.getName).getOrElse("error")))
      /**case e: Reference if Try(e.getOperation.getFeaturingClassifiers.head.getName.forall(isIdChar)).toOption.contains(true)
        && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        Reference(e.getEContainingClass.getName, e.getName, null)**/
    }.toList
  }
}
