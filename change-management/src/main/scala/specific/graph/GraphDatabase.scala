package specific.graph

import java.io.File

import org.neo4j.graphdb.{DynamicRelationshipType, DynamicLabel}
import org.neo4j.{ graphdb => neo4j }

import scala.util.Try


sealed trait GraphDatabase {
  protected val service: neo4j.GraphDatabaseService

  sealed trait Transaction {
    private [GraphDatabase] val tx: neo4j.Transaction
  }

  def transaction[T](f: Transaction => T): Try[T] = {
    Try(new Transaction { private [GraphDatabase] val tx = service.beginTx() }).flatMap { transaction =>
      val res = Try(f(transaction))
      if (res.isFailure) transaction.tx.failure()
      else transaction.tx.success()
      transaction.tx.close()
      res
    }
  }

  object dsl {
    sealed trait Pattern
    sealed trait NodePattern extends Pattern
    case class NodeRef(node: neo4j.Node) extends NodePattern
    case class Node(labels: Seq[String], props: Map[String,Any] = Map.empty) extends NodePattern
    sealed trait RelationshipPattern extends Pattern
    case class RelationshipRef(relationship: neo4j.Relationship) extends RelationshipPattern
    case class Relationship(from: NodePattern, to: NodePattern, label: String, props: Map[String,Any]) extends RelationshipPattern

    def create(pattern: NodePattern)(implicit tx: Transaction): NodeRef = pattern match {
      case Node(labels,props) =>
        val node = service.createNode(labels.map(DynamicLabel.label) :_*)
        props.foreach { case (k,v) => node.setProperty(k,v) }
        NodeRef(node)
      case r: NodeRef => r
    }

    def create(pattern: RelationshipPattern)(implicit tx: Transaction): RelationshipRef = pattern match {
      case Relationship(from, to, label, props) =>
        val NodeRef(f) = create(from)
        val NodeRef(t) = create(to)
        val r = f.createRelationshipTo(t, DynamicRelationshipType.withName(label))
        props.foreach { case (k, v) => r.setProperty(k, v) }
        RelationshipRef(r)
      case r: RelationshipRef => r
    }
  }
}

object GraphDatabase {
  def apply(store: File): GraphDatabase = new GraphDatabase {
    override protected val service: neo4j.GraphDatabaseService = {
      val result = new neo4j.factory.GraphDatabaseFactory().newEmbeddedDatabase(store)
      Runtime.getRuntime.addShutdownHook(new Thread() {
        override def run(): Unit = result.shutdown()
      })
      result
    }
  }

  def apply(store: String = "db"): GraphDatabase = apply(new File(store))
}

