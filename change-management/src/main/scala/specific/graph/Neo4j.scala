package specific.graph

import java.io.File

import org.neo4j.cypher.internal.compiler.v1_9.pipes.matching.NodeIdentifier
import org.neo4j.cypher.{ExecutionResult, ExecutionEngine}
import org.neo4j.graphdb.{Transaction, GraphDatabaseService, SeverityLevel}
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import scala.util.{Failure, Try, Success}
import scala.util.control.NonFatal
import scala.collection.JavaConversions._

/**
 * Created by martin on 06.10.15.
 */
class Neo4j(store: File) {
  lazy val graphDb: GraphDatabaseService = {
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        graphDb.shutdown()
      }
    })
    new GraphDatabaseFactory().newEmbeddedDatabase(store)
  }

  private lazy val executionEngine = new ExecutionEngine(graphDb)

  def shutdown() = graphDb.shutdown()

  def transaction[T](f: Transaction => T): Try[T] = {
    val tx = graphDb.beginTx()
    try {
      val res = f(tx)
      tx.success()
      Success(res)
    } catch {
      case NonFatal(e) =>
        println("error in transaction")
        e.printStackTrace()
        tx.failure()
        Failure(e)
    } finally {
      tx.close()
    }
  }

  def query(query: String, params: Map[String,Any] = Map.empty) = {
    try {
      val result = executionEngine.execute(query, params)
      result.toStream.map(ResultRow(result,_))
    } catch {
      case NonFatal(e) =>
        println("error while executing: " + query)
        e.printStackTrace()
        sys.error("abort")
    }
  }

}

class Node(underlying: org.neo4j.graphdb.Node) {
  val props = underlying.getAllProperties.toMap
  val id = underlying.getId
}

case class ResultRow(result: ExecutionResult, row: Map[String,Any]) {
  def apply[T](column: String)(implicit convert: ResultConverter[T]): T = get[T](column).get
  def get[T](column: String)(implicit convert: ResultConverter[T]): Option[T] =
    row.get(column).flatMap(convert.apply)
}

object Row {
  def unapplySeq(row: ResultRow): Option[Seq[Any]] = Some(row.result.columns.map(row.row.get(_).get))
}

trait ResultConverter[T] {
  def apply(value: Any): Option[T]
  def isDefinedAt(value: Any): Boolean
}

object ResultConverter {
  def apply[T](f: PartialFunction[Any,T]) = new ResultConverter[T] {
    def apply(value: Any) = f.lift(value)
    def isDefinedAt(value: Any) = f.isDefinedAt(value)
  }
}

object Neo4j {
  def apply(path: String = "./db") = new Neo4j(new File(path))

  implicit val BooleanResult = ResultConverter[Boolean]{
    case s: Boolean => s
  }

  implicit val StringResult = ResultConverter[String]{
    case s: String => s
  }

  implicit val LongResult = ResultConverter[Long]{
    case l: Long => l
    case i: Int => i
  }

  implicit val NodeResult = ResultConverter[Node]{
    case n: org.neo4j.graphdb.Node => new Node(n)
  }

  implicit val NodeResultDirect = ResultConverter[org.neo4j.graphdb.Node]{
    case n: org.neo4j.graphdb.Node => n
  }

  implicit def OptionResult[T](implicit convert: ResultConverter[T]) = ResultConverter[Option[T]] {
    case null => None
    case s if convert.isDefinedAt(s) => convert(s)
  }

  implicit def SeqResult[T](implicit convert: ResultConverter[T]) = ResultConverter[Seq[T]] {
    case s: Seq[Any] if s.forall(convert.isDefinedAt) =>
      s.map(convert.apply).map(_.get)
  }
}

trait CypherQuery { self =>
  val query: String
  val params: Map[String,Any] = Map.empty
  def on(ps: (String,Any)*) = new CypherQuery {
    override val query: String = self.query
    override val params: Map[String,Any] = self.params ++ ps
  }
  def apply[T](f: ResultRow => T)(implicit neo4j: Neo4j, tx: Transaction): Seq[T] = {
    val t0 = System.currentTimeMillis()
    val res = neo4j.query(query, params).map(f).force
    val t1 = System.currentTimeMillis()
    val d = t1 - t0
    if (d > 200) println(s"[LONG RUNNING QUERY: $d ms] $query")
    res
  }
  def apply()(implicit neo4j: Neo4j, tx: Transaction): Stream[ResultRow] =
    neo4j.query(query,params)
  def execute()(implicit neo4j: Neo4j, tx: Transaction): Boolean =
    { neo4j.query(query,params); true }
}

object Cypher {
  def apply(q: String) = new CypherQuery {
    override val query: String = q
  }
}