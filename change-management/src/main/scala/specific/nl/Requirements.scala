package specific.nl

import specific.graph._
import Neo4j._
import org.eclipse.emf.ecore.EObject
import specific.Requirement
import scala.io.Source
import scala.collection.mutable.Buffer

/**
 * Created by martin on 10.09.15.
 */
object Requirements {
  private val requirement = "^(\\w+):(.*)$".r

  def load(filename: String): Seq[Requirement] = {
    val s = Source.fromFile(filename).mkString
    s.split("\n").map(_.trim)
      .flatMap(requirement.findFirstMatchIn)
      .map(x => Requirement(x.group(1),x.group(2).trim))
  }

  def writeToGraph(layer: String, requirements: Seq[Requirement])(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    if (requirements.nonEmpty) {
    Cypher(requirements.map { req => s"""
      |MERGE l -[:hasA]-> (r:Requirement { $$TYPE: "Requirement", name: "${req.name}" })
      |ON CREATE SET r.$$STATUS = "ADDED"
      |ON MATCH SET r.$$STATUS = (CASE r.content WHEN "${req.content}" THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET r.content = "${req.content}"
      """.stripMargin
    }.mkString(s"""
      |MERGE (l: Layer {$$TYPE: "Layer", name: "$layer"})
      |ON CREATE SET l.$$STATUS = "ADDED"
      |ON MATCH SET l.$$STATUS = "PRESERVED"
    """.stripMargin, " WITH l ", "")).execute()
  } }

    /*implicit val create = Buffer.empty[String]
    create.append(s"""(r:Semantic:Layer { $$TYPE: "Layer", name: "$layer" } )""")
    create.append("""(rc:Semantic { $$TYPE: "List" } )""")
    requirements.zipWithIndex foreach { case (t,i) =>
      val id = "rc_"+i
      create.append(s"""($id:Semantic:Requirement { $$STATUS: "ADDED", $$TYPE: "Requirement", name: "${t.name}", content: "${t.content}" } )""")
      if (i == 0) create.append(s"(rc -[:NEXT_SIBLING]-> $id)")
      else create.append(s"(rc_${i-1} -[:NEXT_SIBLING]-> $id)")
    }
    val lastId =
      if (requirements.nonEmpty) "rc_" + (requirements.length - 1) else "rc"
    create.append(s"($lastId -[:NEXT_SIBLING]-> rc)")
    create.append("(r -[:CONTENTS]-> rc)")
    val query = Cypher(create.mkString("CREATE ", ", ", ""))
    println(query.execute())*/


  def loadFromGraph(layer: String)(implicit connection: Neo4j): Seq[Requirement] = connection.transaction { implicit tx =>
    val contentsQuery = s"""
      |MATCH (n:Layer:NL) -[:CONTENTS]-> (list),
      |      p = (list) -[:NEXT_SIBLING*0..]-> (list)
      |WHERE n.id = "${layer}"
      |RETURN [node IN nodes(p) WHERE node <> list | node ] as items
    """.stripMargin
    val results = Cypher(contentsQuery)()
    val items = results.flatMap { row => row.get[Seq[Node]]("items").get }
    items.map { node =>
      Requirement(node.props("name").asInstanceOf[String], node.props("content").asInstanceOf[String])
    }
  }.get
}
