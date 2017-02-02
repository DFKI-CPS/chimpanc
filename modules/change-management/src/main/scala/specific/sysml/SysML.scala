package specific.sysml

import org.eclipse.emf.ecore.{EAnnotation, EModelElement, EObject}
import org.eclipse.uml2.uml
import org.eclipse.emf.ecore.resource.Resource
import specific._

import scala.collection.JavaConversions._
import scala.util.Try
import scala.util.parsing.input.Position

/**
  * Created by martin on 1/9/17.
  */
object SysML {
  private def isIdChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  def getEntities(resource: Resource, positions: String => Option[Position]): Set[LayerObject] = {
    resource.getAllContents.collect {
      case e: uml.NamedElement if Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        val path = e.eResource().getURIFragment(e)
        positions(path).map { pos =>
          LayerObject(path, e.getName, pos.line, pos.column)
        }
    }.collect {
      case Some(x: LayerObject) => x
    }.toSet
  }
}
