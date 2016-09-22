package specific.graph.csv

import java.io.{FileWriter, FileOutputStream, StringWriter, File}

import org.eclipse.emf.common.util.{URI, EList}
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.ocl.ecore.Constraint
import scala.collection.JavaConversions._

/**
 * Created by martin on 29.09.15.
 */
class CSV(val filename: String) {
  private val file = new java.io.File(filename)
  private val writer = new FileWriter(file, false)
  private val newLine = System.getProperty("line.separator")

  def writeLine(items: String*) = {
    writer.append(items.mkString(";"))
    writer.append(newLine)
  }

  def close() = writer.close()
}

object CSV {
  def apply() = new CSV(File.createTempFile("specific",".csv").getAbsolutePath)
}