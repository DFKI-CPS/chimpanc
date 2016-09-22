package specific.sysml

import java.io.File

import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore._
import org.eclipse.emf.ecore.resource.{Resource, ResourceSet}
import org.eclipse.emf.emfatic.core.generator.ecore.{Connector, Builder}
import org.eclipse.emf.emfatic.core.lang.gen.parser.EmfaticParserDriver
import specific._

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Try

/**
 * Created by martin on 11.09.15.
 */
object Emfatic {
  private trait EmfaticSourceMapExtractor { self: Builder =>
    def cstDecl2EcoreASTMap = self.getCstDecl2EcoreASTMap
  }

  private def isIdChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  def getEntities(resource: Resource): Seq[LayerObject] = {
    resource.getAllContents.collect {
      case e: EClass if Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        Clazz(e.getName)
      case e: EAttribute if Try(e.getEContainingClass.getName.forall(isIdChar)).toOption.contains(true)
                         && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        Attribute(e.getEContainingClass.getName, e.getName, Option(e.getEType).map(_.getName).getOrElse("void"))
      case e: EOperation if Try(e.getEContainingClass.getName.forall(isIdChar)).toOption.contains(true)
                         && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        Operation(e.getEContainingClass.getName, e.getName, Option(e.getEType).map(_.getName).getOrElse("void"), Try(e.getEParameters.map(p => (p.getName, p.getEType.getName))).getOrElse(Seq.empty))
      case e: EParameter if Try(e.getEOperation.getEContainingClass.getName.forall(isIdChar)).toOption.contains(true)
                         && Try(e.getEOperation.getName.forall(isIdChar)).toOption.contains(true)
                         && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        Parameter(Try(e.getEOperation.getEContainingClass.getName).getOrElse("error"), Try(e.getEOperation.getName).getOrElse("error"), Try(e.getName).getOrElse("error"), Try(e.getEType.getName).getOrElse("error"))
      case e: EReference if Try(e.getEContainingClass.getName.forall(isIdChar)).toOption.contains(true)
                         && Try(e.getName.forall(isIdChar)).toOption.contains(true) =>
        Reference(e.getEContainingClass.getName, e.getName, null)
    }.toList
  }

  def load(filename: String)(implicit resourceSet: ResourceSet): Try[Resource] = Try {
    require(new File(filename).exists(), s"file '$filename' not found")
    val source = Source.fromFile(filename)
    val parser = new EmfaticParserDriver
    val context = parser.parse(source.bufferedReader())
    val builder = new Builder with EmfaticSourceMapExtractor
    val tempFile = new java.io.File(filename + ".ecore").getAbsolutePath
    val resource = resourceSet.createResource(URI.createURI(tempFile))
    val connector = new Connector(builder)
    builder.build(context, resource, new NullProgressMonitor)
    connector.connect(context, resource, new NullProgressMonitor)
    val pkg = resource.getContents.get(0).asInstanceOf[EPackage]
    resourceSet.getPackageRegistry.put(pkg.getNsURI,pkg)
    resource
  }
}
