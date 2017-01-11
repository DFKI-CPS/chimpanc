package specific.graph


import org.eclipse.emf.ecore._
import org.eclipse.ocl.ecore.Constraint
import org.neo4j.graphdb._
import specific.Config

import scala.collection.JavaConversions._
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.emf.common.util.URI
import org.eclipse.uml2.uml
import org.eclipse.uml2.uml.OpaqueExpression

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
object ECoreToGraph {
  def getURI(local: Resource, host: String, obj: EObject): URI = {
    val res = if (obj.eResource() == local) {
      val fragment = EcoreUtil.getRelativeURIFragmentPath(null, obj)
      val newUri = URI.createURI(s"graph://$host#$fragment")
      newUri
    } else EcoreUtil.getURI(obj)
    println(res)
    res
  }

  def writeEObject(obj: EObject, rootId: String)(implicit connection: Neo4j, transaction: Transaction): org.neo4j.graphdb.Node = {
    import connection.{graphDb => db}

    val result = db.createNode(Label.label("EObject"),Label.label("ECore"))

    val features = (for {
      a <- obj.eClass().getEAllStructuralFeatures if obj.eIsSet(a)
    } yield a.getName() -> obj.eGet(a)).toMap + ("eClass" -> getURI(obj.eResource(),rootId,obj.eClass()).toString())

    val fs = features.mapValues[String] {
      case value: EObject =>
        getURI(obj.eResource(),rootId,value).toString
      case value: EList[_] =>
        value.collect {
          case value: EObject =>
            getURI(obj.eResource(),rootId,value).toString
        }.mkString(",")
      //case value: Integer => value
      //case value: java.lang.Boolean => value
      case value => value.toString
    }

    fs.foreach {
      case (k,v) => result.setProperty(k,v)
    }

    val contents = writeEList(obj.eContents(), rootId)
    result.createRelationshipTo(contents, RelationshipType.withName("CONTENTS"))

    result
  }

  def writeEList(list: EList[EObject], rootId: String)(implicit connection: Neo4j, transaction: Transaction): org.neo4j.graphdb.Node = {
    val l = list.filter(Config.filter)
    import connection.{ graphDb => db }
    val result = db.createNode(Label.label("EList"),Label.label("ECore"))
    val items = l.map(writeEObject(_,rootId))
    (result +: items).zip(items :+ result).foreach {
      case (from,to) => from.createRelationshipTo(to, RelationshipType.withName("NEXT_SIBLING"))
    }
    result
  }

  def writeResource(resource: Resource, rootId: String)(implicit connection: Neo4j, transaction: Transaction): org.neo4j.graphdb.Node = {
    import connection.{graphDb => db}

    val root = db.createNode(Label.label("Resource"),Label.label("ECore"))
    root.setProperty("id", rootId)

    val contents = writeEList(resource.getContents, rootId)
    root.createRelationshipTo(contents, RelationshipType.withName("CONTENTS"))
    root
  }
}