package specific.graph

import org.eclipse.emf.common.util.{EList, URI}
import org.eclipse.emf.ecore.{EClass, EObject, EOperation}
import specific._

import scala.util.control.NonFatal
import Neo4j._
import org.eclipse.uml2.uml.{Constraint, Stereotype}

import scala.collection.convert.Wrappers.SeqWrapper

/**
 * @author Martin Ring <martin.ring@dfki.de>
 * @since  Nov 21, 2014
 */ 
object Semantics {
  def deleteDeleted(resource: GraphResource)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    Cypher("""
      |MATCH (:Resource { id: { rootId } }) <-[:ORIGIN]- (:Layer) -[:hasA|parameter*0..4]-> (o{ _STATUS: "DELETED" })
      |MATCH (o) -[r]- () DELETE r, o
    """.stripMargin).on("rootId" -> resource.rootId).execute()
  }

  def resetAdded(resource: GraphResource)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    Cypher("""
      |MATCH (:Resource { id: { rootId } }) <-[:ORIGIN]- (:Layer) -[p:hasA|parameter*0..4]-> (o)
      |SET o._STATUS = "PRESERVED"
    """.stripMargin).on("rootId" -> resource.rootId).execute()
  }
  
  def resetSemantics(resource: GraphResource)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    val rootId = resource.rootId
    Cypher("""
      |MATCH (:Resource { id: { rootId } }) <-[:ORIGIN]- (:Layer) -[p:hasA|parameter*0..4]-> (o)
      |SET o._STATUS = "DELETED"
    """.stripMargin).on("rootId" -> resource.rootId).execute()
  }

  def propagate(resource: GraphResource)(implicit connection: Neo4j, task: TaskOutput) = connection.transaction { implicit tx =>
    Cypher(
      s"""
      |MATCH (resource:Resource { id: "${resource.rootId}" })
      |MERGE (resource) <-[:ORIGIN]- (l:Layer)
      |ON CREATE SET l._STATUS = "ADDED"
      |ON MATCH SET l._STATUS = (CASE [l.name] WHEN [resource.id] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET l.name = resource.id, l._TYPE = "Layer"
      |
      |WITH resource, l
      |MATCH (resource) -[*]-> (origin:EObject { eClass: "http://www.eclipse.org/emf/2002/Ecore#//EClass" } )
      |MERGE (origin) <-[:ORIGIN]- (c:Class)
      |ON CREATE SET c._STATUS = "ADDED"
      |ON MATCH SET c._STATUS = (CASE [c.name] WHEN [origin.name] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET c.name = origin.name, c._TYPE = "Class"
      |MERGE (l) -[:hasA]-> (c)
      |
      |WITH origin as r, c
      |MATCH (r) -[:CONTENTS]-> () -[:NEXT_SIBLING *]-> (origin:EObject { eClass: "http://www.eclipse.org/emf/2002/Ecore#//EOperation" } )
      |MERGE (origin) <-[:ORIGIN]- (o:Operation)
      |ON CREATE SET o._STATUS = "ADDED"
      |ON MATCH SET o._STATUS = (CASE [o.name] WHEN [origin.name] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET o.name = origin.name, o._TYPE = "Operation"
      |MERGE (c) -[:hasA]-> (o)
      |MERGE (o) -[:parameter]-> (p:Parameter { name: "this", position: 0, _TYPE: "Parameter" }) -[:type]-> (c)
      |ON CREATE SET p._STATUS = "ADDED"
      |ON MATCH SET p._STATUS = "PRESERVED"
      |
      |WITH origin as oo, o
      |MATCH (oo) -[:CONTENTS]-> (list) -[:NEXT_SIBLING *]-> (origin:EObject { eClass: "http://www.eclipse.org/emf/2002/Ecore#//EParameter" } ),
      |      path = shortestPath((list) -[:NEXT_SIBLING *]-> (origin))
      |WITH o, origin, count(path) as position
      |MERGE (origin) <-[:ORIGIN]- (p:Parameter)
      |ON CREATE SET p._STATUS = "ADDED"
      |ON MATCH SET p._STATUS = (CASE [p.name, p.position] WHEN [origin.name, position] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET p.name = origin.name, p.position = position, p._TYPE = "Parameter"
      |MERGE (o) -[:parameter]-> (p)""".stripMargin).execute()
    // Propagate Attributes
    Cypher(
      s"""
      |MATCH (resource:Resource { id: "${resource.rootId}" })
      |MATCH (resource) -[*]-> (r:EObject { eClass: "http://www.eclipse.org/emf/2002/Ecore#//EClass" } ) <-[:ORIGIN]- (c:Class)
      |MATCH (r) -[:CONTENTS]-> () -[:NEXT_SIBLING *]-> (origin:EObject { eClass: "http://www.eclipse.org/emf/2002/Ecore#//EAttribute" } )
      |MERGE (origin) <-[:ORIGIN]- (a:Attribute)
      |ON CREATE SET a._STATUS = "ADDED"
      |ON MATCH SET a._STATUS = (CASE [a.name] WHEN [origin.name] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET a.name = origin.name, a._TYPE = "Attribute"
      |MERGE (c) -[:hasA]-> (a)""".stripMargin).execute()
    // Propagate References
    Cypher(
      s"""
      |MATCH (resource:Resource { id: "${resource.rootId}" })
      |MATCH (resource) -[*]-> (r:EObject { eClass: "http://www.eclipse.org/emf/2002/Ecore#//EClass" } ) <-[:ORIGIN]- (c:Class)
      |MATCH (r) -[:CONTENTS]-> () -[:NEXT_SIBLING *]-> (origin:EObject { eClass: "http://www.eclipse.org/emf/2002/Ecore#//EReference" } )
      |MERGE (origin) <-[:ORIGIN]- (ref:Reference)
      |ON CREATE SET ref._STATUS = "ADDED"
      |ON MATCH SET ref._STATUS = (CASE [ref.name, ref.lowerBound, ref.upperBound] WHEN [origin.name, origin.lowerBound, origin.upperBound] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET ref.name = origin.name, ref.lowerBound = origin.lowerBound, ref.upperBound = origin.upperBound, ref._TYPE = "Reference"
      |MERGE (c) -[:hasA]-> (ref)""".stripMargin).execute()
    // Propagate Constraints
    Cypher(
      s"""
      |MATCH (resource:Resource { id: "${resource.rootId}" }) <-[:ORIGIN]- (l:Layer)
      |MATCH (resource) -[*]-> (origin:EObject { eClass: "http://www.eclipse.org/ocl/1.1.0/Ecore#//Constraint" } )
      |MERGE (origin) <-[:ORIGIN]- (c:Constraint { stereotype: origin.stereotype })
      |ON CREATE SET c._STATUS = "ADDED", c.proven = false
      |ON MATCH SET c._STATUS = (CASE [c.name, c.representation] WHEN [origin.name, origin.representation] THEN "PRESERVED" ELSE "MODIFIED" END),
      |             c.proven = (CASE c.representation WHEN origin.representation THEN c.proven ELSE false END)
      |SET c.name = origin.name, c._TYPE = "Constraint", c.representation = origin.representation
      |MERGE (l) -[:hasA]-> (c)
      |WITH l, c
      |MATCH (l) -[:IMPLEMENTS]-> (l2) -[:hasA]-> (d:Constraint)
      |SET d.proven = (d.proven AND c._STATUS = "PRESERVED")""".stripMargin).execute()
    // SysML specific
    Cypher(
      s"""
      |MATCH (resource:Resource { id: "${resource.rootId}" })
      |MERGE (resource) <-[:ORIGIN]- (l:Layer)
      |ON CREATE SET l._STATUS = "ADDED"
      |ON MATCH SET l._STATUS = (CASE [l.name] WHEN [resource.id] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET l.name = resource.id, l._TYPE = "Layer"
      |
      |WITH resource, l
      |MATCH (resource) -[*]-> (origin:EObject { eClass: "http://www.eclipse.org/uml2/5.0.0/UML#//Class" } )
      |MERGE (origin) <-[:ORIGIN]- (c:Class)
      |ON CREATE SET c._STATUS = "ADDED"
      |ON MATCH SET c._STATUS = (CASE [c.name] WHEN [origin.name] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET c.name = origin.name, c._TYPE = "Class"
      |MERGE (l) -[:hasA]-> (c)
      |
      |WITH origin as r, c
      |MATCH (r) -[:CONTENTS]-> () -[:NEXT_SIBLING *]-> (origin:EObject { eClass: "http://www.eclipse.org/uml2/5.0.0/UML#//Operation" } )
      |MERGE (origin) <-[:ORIGIN]- (o:Operation)
      |ON CREATE SET o._STATUS = "ADDED"
      |ON MATCH SET o._STATUS = (CASE [o.name] WHEN [origin.name] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET o.name = origin.name, o._TYPE = "Operation"
      |MERGE (c) -[:hasA]-> (o)
      |MERGE (o) -[:parameter]-> (p:Parameter { name: "this", position: 0, _TYPE: "Parameter" }) -[:type]-> (c)
      |ON CREATE SET p._STATUS = "ADDED"
      |ON MATCH SET p._STATUS = "PRESERVED"
      |
      |WITH origin as oo, o
      |MATCH (oo) -[:CONTENTS]-> (list) -[:NEXT_SIBLING *]-> (origin:EObject { eClass: "http://www.eclipse.org/uml2/5.0.0/UML#//Parameter" } ),
      |      path = shortestPath((list) -[:NEXT_SIBLING *]-> (origin))
      |WITH o, origin, count(path) as position
      |MERGE (origin) <-[:ORIGIN]- (p:Parameter)
      |ON CREATE SET p._STATUS = "ADDED"
      |ON MATCH SET p._STATUS = (CASE [p.name, p.position] WHEN [origin.name, position] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET p.name = origin.name, p.position = position, p._TYPE = "Parameter"
      |MERGE (o) -[:parameter]-> (p)""".stripMargin).execute()
    // Propagate Properties
    Cypher(
      s"""
      |MATCH (resource:Resource { id: "${resource.rootId}" })
      |MATCH (resource) -[*]-> (r:EObject { eClass: "http://www.eclipse.org/uml2/5.0.0/UML#//Class" } ) <-[:ORIGIN]- (c:Class)
      |MATCH (r) -[:CONTENTS]-> () -[:NEXT_SIBLING *]-> (origin:EObject { eClass: "http://www.eclipse.org/uml2/5.0.0/UML#//Property" } )
      |MERGE (origin) <-[:ORIGIN]- (a:Reference)
      |ON CREATE SET a._STATUS = "ADDED"
      |ON MATCH SET a._STATUS = (CASE [a.name] WHEN [origin.name] THEN "PRESERVED" ELSE "MODIFIED" END)
      |SET a.name = origin.name, a._TYPE = "Reference"
      |MERGE (c) -[:hasA]-> (a)""".stripMargin).execute()
    // Propagate Constraints
    Cypher(
      s"""
      |MATCH (resource:Resource { id: "${resource.rootId}" }) <-[:ORIGIN]- (l:Layer)
      |MATCH (resource) -[*]-> (origin:EObject { eClass: "http://www.eclipse.org/uml2/5.0.0/UML#//Constraint" } )
      |MERGE (origin) <-[:ORIGIN]- (c:Constraint)
      |ON CREATE SET c._STATUS = "ADDED", c.proven = false
      |ON MATCH SET c._STATUS = (CASE [c.name, c.representation] WHEN [origin.name, origin.representation] THEN "PRESERVED" ELSE "MODIFIED" END),
      |             c.proven = (CASE c.representation WHEN origin.representation THEN c.proven ELSE false END)
      |SET c.name = origin.name, c._TYPE = "Constraint", c.representation = origin.representation
      |MERGE (l) -[:hasA]-> (c)
      |WITH l, c
      |MATCH (l) -[:IMPLEMENTS]-> (l2) -[:hasA]-> (d:Constraint)
      |SET d.proven = (d.proven AND c._STATUS = "PRESERVED")""".stripMargin).execute()
    val refs = Cypher(
      s"""
      |MATCH (:Layer { name: "${resource.rootId}" }) -[:hasA*]-> (a:Attribute) -[:ORIGIN]-> (origin)
      |RETURN id(a) as from, "type" as relation, origin.eType as to
      |UNION
      |MATCH (:Layer { name: "${resource.rootId}" }) -[:hasA*]-> (r:Reference) -[:ORIGIN]-> (origin)
      |RETURN id(r) as from, "type" as relation, origin.eType as to
      |UNION
      |MATCH (:Layer { name: "${resource.rootId}" }) -[:hasA*]-> (r:Reference) -[:ORIGIN]-> (origin)
      |RETURN id(r) as from, "Opposite" as relation, origin.eOpposite as to
      |UNION
      |MATCH (:Layer { name: "${resource.rootId}" }) -[:hasA*]-> (o:Operation) -[:ORIGIN]-> (origin)
      |RETURN id(o) as from, "type" as relation, origin.eType as to
      |UNION
      |MATCH (:Layer { name: "${resource.rootId}" }) -[:hasA*]-> () -[:parameter]-> (p:Parameter) -[:ORIGIN]-> (origin)
      |RETURN id(p) as from, "type" as relation, origin.eType as to
      |UNION
      |MATCH (:Layer { name: "${resource.rootId}" }) -[:hasA]-> (c:Constraint) -[:ORIGIN]-> (origin)
      |RETURN id(c) as from, "constrainedElement" as relation, origin.constrainedElements as to""".stripMargin){ row =>
      val from = row.get[Long]("from").get
      val relation = row.get[String]("relation").get
      row.get[String]("to").filter(_ != "<null>").fold {
        s"""
         |MATCH (f) WHERE id(f) = $from
         |MERGE (null:Null { name: "void", _STATUS: "PRESERVED" }) -[:IMPLEMENTS]-> (null)
         |MERGE (f) -[r:$relation]-> (null)
         |ON CREATE SET r._STATUS = "ADDED"
         |ON MATCH SET r._STATUS = "PRESERVED"
         |RETURN NULL
        """.stripMargin
      } { (to: String) =>
        val uri = URI.createURI(to)
        resource.getResourceSet().getEObject(uri,true) match {
          case g: GraphEObject =>
            s"""
             |MATCH (f) WHERE id(f) = $from
             |MATCH (to) <-[:ORIGIN]- (t) WHERE id(to) = ${g.node.id}
             |MERGE (f) -[r:$relation]-> (t)
             |ON CREATE SET r._STATUS = "ADDED"
             |ON MATCH SET r._STATUS = "PRESERVED"
             |RETURN NULL""".stripMargin
          case other: EObject if other.eClass().getEStructuralFeature("name") != null =>
            val name = other.eGet(other.eClass().getEStructuralFeature("name")).asInstanceOf[String]
            s"""
             |MATCH (f) WHERE id(f) = $from
             |MERGE (null:External { name: "$name", uri: "$to", _STATUS: "PRESERVED"  }) -[:IMPLEMENTS]-> (null)
             |MERGE (f) -[r:$relation]-> (null)
             |ON CREATE SET r._STATUS = "ADDED"
             |ON MATCH SET r._STATUS = "PRESERVED"
             |RETURN NULL""".stripMargin
          case other =>
            s"""
             |MATCH (f) WHERE id(f) = $from
             |MERGE (null:External { uri: "$to", _STATUS: "PRESERVED"  }) -[:IMPLEMENTS]-> (null)
             |MERGE (f) -[r:$relation]-> (null)
             |ON CREATE SET r._STATUS = "ADDED"
             |ON MATCH SET r._STATUS = "PRESERVED"
             |RETURN NULL""".stripMargin
        }
      }
    }
    refs.foreach(println)
    refs.foreach(Cypher(_).execute())
  }.get

  def autoMap()(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    Cypher("""
     |MATCH (l1:Layer) -[:IMPLEMENTS]-> (l2:Layer),
     |      (l2) -[:hasA *..3]-> (m:Class)
     |WHERE NOT () -[:IMPLEMENTS]-> (m)
     |MATCH (l1) -[:hasA *..3]-> (n:Class)
     |WHERE n.name = m.name
     |  AND n._STATUS <> "DELETED" AND m._STATUS <> "DELETED"
     |CREATE UNIQUE (n) -[:IMPLEMENTS]-> (m)""".stripMargin).execute()
    Cypher("""
     |MATCH (n:Class) -[:IMPLEMENTS]-> (m:Class),
     |      (m) -[:hasA]-> (q)
     |WHERE NOT () -[:IMPLEMENTS]-> (q)
     |MATCH (n) -[:hasA]-> (p) WHERE p.name = q.name
     |MATCH (p) -[:type]-> (pt) -[:IMPLEMENTS]-> (qt) <-[:type]- (q)
     |WHERE p._STATUS <> "DELETED" AND q._STATUS <> "DELETED"
     |  AND pt._STATUS <> "DELETED" AND qt._STATUS <> "DELETED"
     |CREATE UNIQUE (p) -[:IMPLEMENTS]-> (q)""".stripMargin).execute()
    Cypher("""
     |MATCH (n:Operation) -[:IMPLEMENTS]-> (m:Operation),
     |      (m) -[:parameter]-> (q)
     |WHERE NOT () -[:IMPLEMENTS]-> (q)
     |MATCH (n) -[:parameter]-> (p),
     |      (p) -[:type]-> (pt) -[:IMPLEMENTS]-> (qt) <-[:type]- (q)
     |WHERE p.position = q.position
     |  AND p._STATUS <> "DELETED" AND q._STATUS <> "DELETED"
     |  AND pt._STATUS <> "DELETED" AND qt._STATUS <> "DELETED"
     |CREATE UNIQUE (p) -[:IMPLEMENTS]-> (q)""".stripMargin).execute()
  }

  def matchEntity(name: String, layer: String, entity: LayerObject): String = entity match {
    case PositionedLayerObject(x,_,_) => matchEntity(name,layer,x)
    case Clazz(clazz,_) =>
      s"""(:Layer { name: "$layer" }) -[:hasA *..2]->
         |($name:Class { name: "$clazz" })""".stripMargin
    case Attribute(clazz,m,_) =>
      s"""(:Layer { name: "$layer" }) -[:hasA *..2]->
         |(:Class { name: "$clazz" }) -[:hasA]->
         |($name:Attribute { name: "$m" })""".stripMargin
    case Operation(clazz,m,_,_) =>
      s"""(:Layer { name: "$layer" }) -[:hasA *..2]->
         |(:Class { name: "$clazz" }) -[:hasA]->
         |($name:Operation { name: "$m" })""".stripMargin
    case Parameter(clazz,m,n,_) =>
      s"""(:Layer { name: "$layer" }) -[:hasA *..2]->
         |(:Class { name: "$clazz" }) -[:hasA]->
         |(:Operation { name: "$m" }) -[:parameter]->
         |($name:Parameter { name: "$n" })""".stripMargin
    case Reference(clazz,m,_) =>
      s"""(:Layer { name: "$layer" }) -[:hasA *..2]->
         |(:Class { name: "$clazz" }) -[:hasA]->
         |($name:Reference { name: "$m" })""".stripMargin
    case Requirement(rname, _) =>
      s"""(:Layer { name: "$layer" }) -[:hasA]-> ($name:Requirement { name: "$rname" })"""
    case Precondition(_,pname) =>
      s"""(:Layer { name: "$layer" }) -[:hasA]->
         |($name { stereotype: "precondition", name: "$pname" })""".stripMargin
    case Postcondition(_,pname) =>
      s"""(:Layer { name: "$layer" }) -[:hasA]->
         |($name:Constraint { stereotype: "postcondition", name: "$pname" })""".stripMargin
    case Invariant(_,pname) =>
      s"""(:Layer { name: "$layer" }) -[:hasA]->
         |($name:Constraint { stereotype: "invariant", name: "$pname" })""".stripMargin
  }

  def createMapping(fromLayer: String, toLayer: String)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    Cypher(
    s"""MATCH (from:Layer { name: "$fromLayer" }),
       |      (to:Layer { name: "$toLayer" })
       |CREATE UNIQUE (from)-[:IMPLEMENTS]->(to)
     """.stripMargin).execute()
  }.get

  def createMapping(fromLayer: String, from: LayerObject, toLayer: String, to: LayerObject)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    val fromMatch = matchEntity("from",fromLayer,from)
    val toMatch = matchEntity("to",toLayer,to)
    val q = s"MATCH $fromMatch, $toMatch CREATE UNIQUE (from)-[:IMPLEMENTS]->(to)"
    Cypher(q)()
  }

  def removeMappings(layer: String, to: LayerObject)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    val toMatch = matchEntity("to",layer,to)
    val q = s"MATCH $toMatch, (to) <-[r:IMPLEMENTS]- () DELETE r"
    Cypher(q)()
  }

  private def constr(st: String, n: String) = st match {
    case "precondition" => Precondition(null,n)
    case "postcondition" => Postcondition(null,n)
    case "invariant" => Invariant(null,n)
  }

  private def membr(t: String, c: String, n: String) = t match {
    case "Attribute" => Attribute(c, n, null)
    case "Reference" => Reference(c, n, null)
    case "Operation" => Operation(c, n, null, null)
  }

  implicit val rowToLayerObject = ResultConverter[LayerObject] (({
    case xs: Seq[AnyRef] => xs
    case xs: SeqWrapper[AnyRef] => xs.underlying
  }: PartialFunction[AnyRef,Seq[AnyRef]]).andThen({
      case Seq("Constraint", name: String, _, _, _, "invariant") => Invariant(null, name)
      case Seq("Constraint", name: String, _, _, _, "precondition") => Precondition(null, name)
      case Seq("Constraint", name: String, _, _, _, "postcondition") => Postcondition(null, name)
      case Seq("Class", name: String, _, _, _, _) => Clazz(name, None)
      case Seq("Attribute", name: String, clazz: String, _, t: String, _) => Attribute(clazz, name, Option(t).getOrElse("<unknown>"))
      case Seq("Attribute", name: String, clazz: String, _, _, _) => Attribute(clazz, name, "<unknown>")
      case Seq("Reference", name: String, clazz: String, _, t: String, _) => Reference(clazz, name, Option(t).getOrElse("<unknown>"))
      case Seq("Reference", name: String, clazz: String, _, _, _) => Reference(clazz, name, "<unknown>")
      case Seq("Operation", name: String, clazz: String, _, t: String, _) => Operation(clazz, name, Option(t).getOrElse("<unknown>"), null)
      case Seq("Operation", name: String, clazz: String, _, _, _) => Operation(clazz, name, "<unknown>", null)
      case Seq("Parameter", name: String, clazz: String, op: String, t: String, _) => Parameter(clazz, op, name, Option(t).getOrElse("<unknown>"))
      case Seq("Requirement", name: String, _, _, _, _) => Requirement(name, null)
    })
  )

  def listMappings()(implicit connection: Neo4j): Seq[Mapping] = connection.transaction { implicit tx =>
    Cypher("""
        |MATCH (a) -[:IMPLEMENTS]-> (b)
        |WHERE a.name IS NOT NULL AND b.name IS NOT NULL
        |  AND NOT a:Layer AND NOT b:Layer
        |  AND a._STATUS <> "DELETED" AND b._STATUS <> "DELETED"
        |MATCH (layerA:Layer) -[:hasA|parameter*1..4]-> (a)
        |MATCH (layerB:Layer) -[:hasA|parameter*1..4]-> (b)
        |OPTIONAL MATCH (classA:Class) -[:hasA|parameter*1..2]-> (a)
        |OPTIONAL MATCH (classB:Class) -[:hasA|parameter*1..2]-> (b)
        |OPTIONAL MATCH (opA:Operation) -[:parameter]-> (a)
        |OPTIONAL MATCH (opB:Operation) -[:parameter]-> (b)
        |OPTIONAL MATCH (a) -[:type]-> (at)
        |OPTIONAL MATCH (b) -[:type]-> (bt)
        |RETURN layerA.name as fromLayer, [a._TYPE, a.name, classA.name, opA.name, at.name, a.stereotype] as from,
        |       layerB.name as toLayer, [b._TYPE, b.name, classB.name, opB.name, bt.name, b.stereotype] as to
      """.stripMargin) {
      case row => Mapping(
        row.get[String]("fromLayer").get,
        row.get[LayerObject]("from").get,
        row.get[String]("toLayer").get,
        row.get[LayerObject]("to").get)
    }.toList
  }.get

  def ignoreModel(layer: String, model: LayerObject, reason: String)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    val nMatch = matchEntity("n",layer,model)
    Cypher(s"""MATCH $nMatch SET n._IGNORE = "$reason"""").execute()
  }

  def proven(layer: String, constr: String)(implicit connection: Neo4j) = connection.transaction { implicit tx =>
    Cypher(s"""
     |MATCH (n:Layer { name: "$layer" }) -[:hasA]-> (c:Constraint { name: "$constr" } )
     |SET c.proven = true
    """.stripMargin
    ).execute()
  }

  def constraints(layer: String)(implicit connection: Neo4j): Seq[(Clazz,ConstraintInfo,Boolean)] = connection.transaction { implicit tx =>
    Cypher( s"""
     |MATCH (:Layer { name: "$layer" }) -[:hasA]-> (c:Constraint)
     |      -[:constrainedElement]-> () <-[:hasA *0..1]- (cl:Class)
     |RETURN DISTINCT cl.name, c.name, c.stereotype, c.proven
    """.stripMargin
    ) {
      case Row(cl: String, n: String, st: String, proven: Boolean) =>
        (Clazz(cl,None),constr(st,n),proven)
    }
  }.get

  implicit class TrySeq[T](val
                           seq: Seq[T]) extends AnyVal {
    def tryMap[U](f: T => U): Seq[U] = seq.map { t =>
      try {
        Some(f(t))
      } catch {
        case NonFatal(t) =>
          t.printStackTrace()
          Option.empty[U]
      }
    }.flatten
  }

  def findProblems()(implicit connection: Neo4j): Seq[SemanticIssue] = connection.transaction { implicit tx =>
    Cypher("""
      |MATCH (ap) -[:IMPLEMENTS]-> (bp)
      |WHERE ap._STATUS <> "DELETED" AND bp._STATUS <> "DELETED"
      |MATCH (bp) -[:hasA|parameter]-> (b)
      |WHERE b._STATUS <> "DELETED" AND NOT EXISTS(b._IGNORE)
      |  AND NOT () -[:IMPLEMENTS]-> (b) AND NOT b:Constraint
      |  AND NOT b:Requirement
      |MATCH (b) <-[:hasA|parameter*1..4]- (layerB:Layer)
      |WHERE layerB._STATUS <> "DELETED"
      |OPTIONAL MATCH (classB:Class) -[:hasA|parameter*1..2]-> (b)
      |OPTIONAL MATCH (opB:Operation) -[:parameter]-> (b)
      |OPTIONAL MATCH (b) -[:type]-> (bt)
      |RETURN layerB.name as layer, [b._TYPE, b.name, classB.name, opB.name, bt.name, b.stereotype] as model
    """.stripMargin) { row =>
      println(row.get[String]("layer"))
      println(row.row.get("model"))
      println(row.get[LayerObject]("model"))
      UnimplementedModel(row.get[String]("layer").get, row.get[LayerObject]("model").get)
    } ++ Cypher("""
      |MATCH (a) -[:IMPLEMENTS]-> (b) -[:type]-> (bt) <-[:IMPLEMENTS]- (expected)
      |WHERE a._STATUS <> "DELETED" AND b._STATUS <> "DELETED"
      |  AND bt._STATUS <> "DELETED" AND expected._STATUS <> "DELETED"
      |  AND NOT expected.name IS NULL
      |MATCH (a) -[:type] -> (actual)
      |WHERE actual._STATUS <> "DELETED"
      |  AND actual <> expected
      |  AND NOT actual.name IS NULL
      |MATCH (b) <-[:hasA|parameter*1..4]- (layerB:Layer)
      |OPTIONAL MATCH (classB:Class) -[:hasA|parameter*1..2]-> (b)
      |OPTIONAL MATCH (opB:Operation) -[:parameter]-> (b)
      |RETURN layerB.name as layer, [b._TYPE, b.name, classB.name, opB.name, bt.name, b.stereotype] as model,
      |       expected.name as expected, actual.name as actual
    """.stripMargin) { row =>
      MaltypedMapping(row.get[String]("layer").get,
                      row.get[LayerObject]("model").get.asInstanceOf[Entity],
                      row.get[String]("expected").get, row.get[String]("actual").get)
    } ++ Cypher("""
      |MATCH (layerA:Layer) -[:hasA|parameter*1..4]-> (a) -[:IMPLEMENTS]-> (b { _STATUS: "DELETED" }) <-[:hasA|parameter*1..4]- (layerB:Layer)
      |WHERE a._STATUS <> "DELETED"
      |OPTIONAL MATCH (classA:Class) -[:hasA|parameter*1..2]-> (a)
      |OPTIONAL MATCH (classB:Class) -[:hasA|parameter*1..2]-> (b)
      |OPTIONAL MATCH (opA:Operation) -[:parameter]-> (a)
      |OPTIONAL MATCH (opB:Operation) -[:parameter]-> (b)
      |OPTIONAL MATCH (a) -[:type]-> (at)
      |OPTIONAL MATCH (b) -[:type]-> (bt)
      |RETURN layerA.name as layer2, [a._TYPE, a.name, classA.name, opA.name, at.name, a.stereotype] as implementation,
      |       layerB.name as layer, [b._TYPE, b.name, classB.name, opB.name, bt.name, b.stereotype] as model
    """.stripMargin) { row =>
      RemovedModel(row.get[String]("layer").get, row.get[LayerObject]("model").get,
                   row.get[String]("layer2").get, row.get[LayerObject]("implementation").get)
    } ++ Cypher("""
      |MATCH (l:Layer) -[:hasA|parameter*1..4]-> (a)
      |WHERE EXISTS (a._IGNORE)
      |OPTIONAL MATCH (c:Class) -[:hasA|parameter*1..2]-> (a)
      |OPTIONAL MATCH (op:Operation) -[:parameter]-> (a)
      |OPTIONAL MATCH (a) -[:type]-> (at)
      |RETURN l.name as layer, [a._TYPE, a.name, c.name, op.name, at.name, a.stereotype] as model, a._IGNORE as reason
    """.stripMargin) { row =>
      IgnoredModel(row.get[String]("layer").get, row.get[LayerObject]("model").get,
                   row.get[String]("reason").get)
    } ++ Cypher("""
      |MATCH (layerA:Layer) -[:hasA|parameter*1..4]-> (a { _STATUS: "DELETED" }) -[:IMPLEMENTS]-> (b) <-[:hasA|parameter*1..4]- (layerB:Layer)
      |WHERE b._STATUS <> "DELETED"
      |OPTIONAL MATCH (classA:Class) -[:hasA|parameter*1..2]-> (a)
      |OPTIONAL MATCH (classB:Class) -[:hasA|parameter*1..2]-> (b)
      |OPTIONAL MATCH (opA:Operation) -[:parameter]-> (a)
      |OPTIONAL MATCH (opB:Operation) -[:parameter]-> (b)
      |OPTIONAL MATCH (a) -[:type]-> (at)
      |OPTIONAL MATCH (b) -[:type]-> (bt)
      |RETURN layerA.name as layer2, [a._TYPE, a.name, classA.name, opA.name, at.name, a.stereotype] as implementation,
      |       layerB.name as layer, [b._TYPE, b.name, classB.name, opB.name, bt.name, b.stereotype] as model
    """.stripMargin) { row =>
      RemovedImplementation(row.get[String]("layer").get, row.get[LayerObject]("model").get,
                            row.get[String]("layer2").get, row.get[LayerObject]("implementation").get)
    } ++ Cypher("""
      |MATCH (layerA:Layer) -[:hasA|parameter*1..4]-> (a) -[:IMPLEMENTS]-> (b:Requirement) <-[:hasA|parameter*1..4]- (layerB:Layer)
      |WHERE b._STATUS <> "DELETED"
      |MATCH (a) -[:hasA|parameter*0..4]-> (x)
      |WHERE x._STATUS <> "PRESERVED"
      |OPTIONAL MATCH (classA:Class) -[:hasA|parameter*1..2]-> (a)
      |OPTIONAL MATCH (classB:Class) -[:hasA|parameter*1..2]-> (b)
      |OPTIONAL MATCH (opA:Operation) -[:parameter]-> (a)
      |OPTIONAL MATCH (opB:Operation) -[:parameter]-> (b)
      |OPTIONAL MATCH (a) -[:type]-> (at)
      |OPTIONAL MATCH (b) -[:type]-> (bt)
      |RETURN layerA.name as layer2, [a._TYPE, a.name, classA.name, opA.name, at.name, a.stereotype] as implementation,
      |       layerB.name as layer, [b._TYPE, b.name, classB.name, opB.name, bt.name, b.stereotype] as model
    """.stripMargin) { row =>
      ModifiedImplementation(row.get[String]("layer").get, row.get[LayerObject]("model").get,
        row.get[String]("layer2").get, row.get[LayerObject]("implementation").get)
    } ++ Cypher("""
      |MATCH (refA:Reference) -[:IMPLEMENTS]-> (refB:Reference)
      |WHERE refA._STATUS <> "DELETED" AND refB._STATUS <> "DELETED"
      |  AND (refA.lowerBound <> refB.lowerBound OR refA.upperBound <> refB.upperBound)
      |MATCH (refB) <-[:hasA]- (classB:Class) <-[:hasA]- (layerB:Layer)
      |RETURN layerB.name as layer, [refB._TYPE, refB.name, classB.name, NULL, NULL, NULL] as model,
      |       refA.lowerBound, refA.upperBound, refB.lowerBound, refB.upperBound
    """.stripMargin) { row =>
      MismatchingBounds(row.get[String]("layer").get, row.get[LayerObject]("model").get, null, null)
    }
  }.get
}