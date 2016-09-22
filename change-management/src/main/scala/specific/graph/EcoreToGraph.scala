package specific.graph

import java.io.File
import java.util.UUID

import org.eclipse.emf.ecore._
import org.eclipse.ocl.ecore.{Constraint, ExpressionInOCL}
import org.neo4j.graphdb.{Transaction, DynamicRelationshipType, DynamicLabel, Label}
import specific.Config
import specific.graph.csv.CSV
import scala.collection.JavaConversions._
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.util.EcoreUtil
import scala.collection.mutable.Buffer
import org.eclipse.emf.common.util.URI

import scala.util.Try

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
object ECoreToGraph {
  def getURI(local: Resource, host: String, obj: EObject): URI = {
    if (obj.eResource() == local) {
      val oldUri = EcoreUtil.getURI(obj)
      val newUri = URI.createURI(s"graph://$host#${oldUri.fragment()}")
      newUri
    } else EcoreUtil.getURI(obj)
  }

  def writeEObject(obj: EObject, rootId: String)(implicit connection: Neo4j, transaction: Transaction): org.neo4j.graphdb.Node = {
    import connection.{graphDb => db}
    import DynamicRelationshipType.{ withName => relation }
    import DynamicLabel.label

    val result = db.createNode(label("EObject"),label("ECore"))

    val features = (for {
      a <- obj.eClass().getEAllStructuralFeatures if obj.eIsSet(a)
    } yield a.getName() -> obj.eGet(a)).toMap ++ (obj match {
      case c: Constraint => Some("representation" -> c.toString)
      case _ => None
    }) + ("eClass" -> getURI(obj.eResource(),rootId,obj.eClass()).toString())

    val fs = features.mapValues[String] {
      case value: EObject =>
        getURI(obj.eResource(),rootId,value).toString
      case value: EList[EObject] =>
        value.map(value => getURI(obj.eResource(),rootId,value).toString).mkString(",")
      //case value: Integer => value
      //case value: java.lang.Boolean => value
      case value => value.toString
    }

    fs.foreach {
      case (k,v) => result.setProperty(k,v)
    }

    val contents = writeEList(obj.eContents(), rootId)
    result.createRelationshipTo(contents, relation("CONTENTS"))

    result
  }

  def writeEList(list: EList[EObject], rootId: String)(implicit connection: Neo4j, transaction: Transaction): org.neo4j.graphdb.Node = {
    val l = list.filter(Config.filter)
    import DynamicLabel.label
    import DynamicRelationshipType.{ withName => relation }
    import connection.{ graphDb => db }
    val result = db.createNode(label("EList"),label("ECore"))
    val items = l.map(writeEObject(_,rootId))
    (result +: items).zip(items :+ result).foreach {
      case (from,to) => from.createRelationshipTo(to, relation("NEXT_SIBLING"))
    }
    result
  }

  def writeResource(resource: Resource, rootId: String)(implicit connection: Neo4j, transaction: Transaction): org.neo4j.graphdb.Node = {
    import DynamicLabel.label
    import DynamicRelationshipType.{ withName => relation }
    import connection.{ graphDb => db }
    val root = db.createNode(label("Resource"),label("ECore"))
    root.setProperty("id", rootId)

    val contents = writeEList(resource.getContents, rootId)
    root.createRelationshipTo(contents, relation("CONTENTS"))
    root
  }
}