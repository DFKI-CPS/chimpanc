package specific.systemc

import java.io.File
import java.lang.ProcessBuilder
import java.util

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EcorePackage.Literals
import org.eclipse.emf.ecore.{EPackage, EOperation, EcoreFactory, EClass}
import org.eclipse.emf.ecore.impl.EClassImpl
import org.eclipse.emf.ecore.resource.{ResourceSet, Resource}
import specific.Config
import specific.sysml.Emfatic
import sun.font.EAttribute

import scala.util.Try
import scala.collection.JavaConversions._
import scala.sys.process._
import scala.io.Source
import scala.util.matching.Regex

/**
 * Created by martin on 11.09.15.
 */
object SystemC {
  def getEntities(resource: Resource) = Emfatic.getEntities(resource)
}
