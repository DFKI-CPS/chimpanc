package specific.project

import java.io.{FileWriter, File}

import org.neo4j.cypher.InvalidArgumentException
import specific.Specs

import scala.io.Source

/**
 * Created by martin on 09.09.15.
 */
case class Project(layers: Seq[Layer]) {
  require(layers.isEmpty || !layers.tail.exists(_.isInstanceOf[Layer.NL]), "only the first layer can be a nl layer")
  require(layers.isEmpty || !layers.init.exists(_.isInstanceOf[Layer.ESL]), "only the last layer can be a esl layer")

  lazy val nl: Option[Layer.NL] = layers.headOption.collect { case nl: Layer.NL => nl }
  lazy val fsls: Seq[Layer.FSLLayer] = layers.collect { case fsl: Layer.FSLLayer => fsl}
  lazy val esl: Option[Layer.ESL] = layers.lastOption.collect { case esl: Layer.ESL => esl }

  def toSpecs = Specs(
    nl.map(nl => specific.NL(nl.name, nl.source)),
    fsls.map {
      case sysml: Layer.SysML => specific.SysML(sysml.name, sysml.source)
      case fsl: Layer.FSL => specific.FSL(fsl.name, fsl.emfSource, fsl.oclSource)
    },
    esl.map(esl => specific.ESL(esl.name, esl.source))
  )
}

object Project {
  def parse(file: String): Project = {
    val path = new File(file).getParent
    val source = scala.io.Source.fromFile(file)
    import scala.util.parsing.json._
    JSON.parseFull(source.mkString) match {
      case Some(map: Map[String,Any]) =>
        map.get("layers") match {
          case Some(list: List[Any]) =>
            Project(list.map(Layer.parse(_,path)))
          case _ => throw new InvalidArgumentException("layers is not an array")
        }
    }
  }
}

sealed trait Layer {
  val name: String
}

object Layer {
  def writeFile(path: String, content: String): Unit = {
    val writer = new FileWriter(path)
    writer.write(content)
    writer.close()
  }

  def parse(a: Any, path: String): Layer = {
    import scala.util.parsing.json._
    a match {
      case map: Map[String,Any] =>
        val files = map("files").asInstanceOf[List[String]]
        val name = map("name").asInstanceOf[String]
        map("type").asInstanceOf[String] match {
          case "nl" => NL(name, path + "/" + files(0))
          case "fsl" => FSL(name, path + "/" + files(0), path + "/" + files(1))
          case "sysml" => SysML(name, path + "/" + files(0))
          case "esl" => ESL(name, path + "/" + files(0))
        }
    }
  }

  case class NL(name: String, file: String) extends Layer {
    require(new File(file).exists())
    def source: String = scala.io.Source.fromFile(file).mkString
    def source_=(value: String) = Layer.writeFile(file,value)
  }

  sealed trait FSLLayer extends Layer

  case class SysML(name: String, file: String) extends FSLLayer {
    require(new File(file).exists())
    def source: String = scala.io.Source.fromFile(file).mkString
    def source_=(value: String) = Layer.writeFile(file,value)
  }

  case class FSL(name: String, emfFile: String, oclFile: String) extends FSLLayer {
    require(new File(emfFile).exists())
    require(new File(oclFile).exists())
    def emfSource: String = scala.io.Source.fromFile(emfFile).mkString
    def emfSource_=(value: String) = Layer.writeFile(emfFile,value)
    def oclSource: String = scala.io.Source.fromFile(oclFile).mkString
    def oclSource_=(value: String) = Layer.writeFile(oclFile,value)
    def getStateChart(clazz: String): Option[String] = {
      val file = new File((new File(emfFile).getParent) + s"/$name-$clazz-states.svg")
      if (file.exists()) Some(Source.fromFile(file).mkString) else None
    }
  }

  case class ESL(name: String, file: String) extends Layer {
    require(new File(file).exists())
    def source: String = scala.io.Source.fromFile(file).mkString
    def source_=(value: String) = Layer.writeFile(file,value)
  }
}