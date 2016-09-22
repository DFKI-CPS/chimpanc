package specific.graph

import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.EObject

import scala.collection.mutable

/**
 * @author Martin Ring <martin.ring@dfki.de>
 */
class HybridResourceSet(implicit connection: Neo4j) extends ResourceSetImpl {
  override def getResource(uri: URI, loadOnDemand: Boolean): Resource = uri.scheme match {
    case "graph" =>
      getGraphResource(uri.host(), loadOnDemand)
    case _ => super.getResource(uri, loadOnDemand)
  }

  override def getEObject(uri: URI, loadOnDemand: Boolean): EObject = uri.scheme match {
    case "graph" =>
      /*var xn = 0
      def name = s"x$xn"
      def nextName = { xn += 1; name }
      val matchExpr = StringBuilder.newBuilder
      val whereExpr = mutable.Buffer.empty[String]
      matchExpr.append(s"(:Resource { id: '${uri.host}' }) -[:CONTENTS]-> ()")
      uri.fragment().split(Array('/','.')).filter(_.nonEmpty).foreach {
        case n if n.forall(_.isDigit) => n.toInt match {
          case 0 => matchExpr.append(s" -[:NEXT_SIBLING]-> ($nextName)")
          case n => matchExpr.append(s" -[:NEXT_SIBLING *$n]-> ($nextName)")
        }
        case n =>
          matchExpr.append(s" -[:NEXT_SIBLING *]-> ($nextName")
          whereExpr.append(s"$name.name = '$n'")
      }
      val where = if (whereExpr.isEmpty) "" else whereExpr.mkString("WHERE ", " AND ", "")*/
      val resource = getGraphResource(uri.host(), loadOnDemand)
      val fragment = uri.fragment() match {
        case f if f.startsWith("/") => f.tail
        case f => f
      }
      val parts = fragment.split(Array('/','.'))
      resource.getGraphObject(parts) match {
        case null =>
          println(s"[warn:] uri $uri resolved to null")
          null
        case other => other
      }
    case _ => super.getEObject(uri, loadOnDemand)
  }
  
  def getGraphResource(rootId: String, loadOnDemand: Boolean = true): GraphResource = {
    new GraphResource(rootId, this, loadOnDemand)
  }
  
  def clearGraph() = {
    connection.transaction { implicit tx =>
      Cypher(
        """
          |MATCH (a)
          |WITH a
          |LIMIT 10000
          |OPTIONAL MATCH (a)-[r]-()
          |DELETE a,r
          |RETURN COUNT(*)
        """.stripMargin).execute()
    }
    connection.transaction { implicit tx =>
      Cypher("CREATE CONSTRAINT ON (e:ECore) ASSERT e.uid IS UNIQUE").execute()
      Cypher("CREATE INDEX ON :EObject(eClass)").execute()
      Cypher("CREATE INDEX ON :Layer(name)").execute()
      Cypher("CREATE INDEX ON :Class(name)").execute()
      Cypher("CREATE INDEX ON :Attribute(name)").execute()
      Cypher("CREATE INDEX ON :Reference(name)").execute()
      Cypher("CREATE INDEX ON :Operation(name)").execute()
      Cypher("CREATE INDEX ON :Parameter(name)").execute()
      Cypher("CREATE INDEX ON :Constraint(name)").execute()
    }
  }
}