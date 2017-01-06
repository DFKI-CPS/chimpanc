package specific

/**
 * @author martin
 */

sealed trait SemanticIssue
case class RequirementEvaluation(layer: String, req: Requirement, results: Seq[Option[Boolean]]) extends SemanticIssue
case class UnimplementedModel(layer: String, model: LayerObject) extends SemanticIssue
case class MaltypedMapping(layer: String, model: Entity, expected: String, actual: String) extends SemanticIssue
case class IgnoredModel(layer: String, model: LayerObject, reason: String) extends SemanticIssue
case class RemovedModel(layer: String, model: LayerObject, otherLayer: String, implementation: LayerObject) extends SemanticIssue
case class RemovedImplementation(layer: String, model: LayerObject, otherLayer: String, implementation: LayerObject) extends SemanticIssue
case class ModifiedImplementation(layer: String, model: LayerObject, otherLayer: String, implementation: LayerObject) extends SemanticIssue
case class MismatchingBounds(layer: String, model: LayerObject, expected: String, actual: String) extends SemanticIssue
case class OCLProofObligation(layer: String, clazz: String, premise: Seq[String], implication: String, proven: Boolean) extends SemanticIssue

sealed trait Spec { val name: String }
case class NL(name: String, content: String) extends Spec
sealed trait FSLSpec extends Spec
case class SysML(name: String, content: String) extends FSLSpec
case class FSL(name: String, emf: String, ocl: String) extends FSLSpec
case class ESL(name: String, systemc: String) extends Spec

case class Specs(nl: Option[NL], fsl: Seq[FSLSpec], esl: Option[ESL])

sealed trait LayerObject {
  val name: String
  def qName: String
  override def toString = qName
}

case class PositionedLayerObject(obj: LayerObject, line: Int, column: Int) extends LayerObject {
  val name = obj.name
  def qName = obj.qName

  override def toString: String = s"$obj at [$line:$column]"
}

sealed trait Entity extends LayerObject
case class Clazz(name: String, stateChart: Option[String] = None) extends Entity { def qName = name }

case class Operation(clazz: String, name: String, tpe: String, params: Seq[(String,String)]) extends Entity { def qName = clazz + "." + name }
case class Attribute(clazz: String, name: String, tpe: String) extends Entity { def qName = clazz + "." + name }
case class Reference(clazz: String, name: String, tpe: String) extends Entity { def qName = clazz + "." + name }

case class Parameter(clazz: String, op: String, name: String, tpe: String) extends Entity { def qName = s"$clazz.$op($name)" }

sealed trait NLObject extends LayerObject
case class Requirement(name: String, content: String) extends NLObject { def qName = name }

sealed trait ConstraintInfo extends LayerObject { def constrainedElement: LayerObject; val name: String; override def qName = "constraint:" + name }
case class Invariant(clazz: Clazz, name: String) extends ConstraintInfo { def constrainedElement = clazz }
case class Precondition(op: Operation, name: String) extends ConstraintInfo { def constrainedElement = op }
case class Postcondition(op: Operation, name: String) extends ConstraintInfo { def constrainedElement = op }

sealed trait SemanticInfo
case class Mapping(fromLayer: String, from: LayerObject, toLayer: String, to: LayerObject) extends SemanticInfo

sealed trait Message
case class Init(nlp: String, fsl: String, esl: String) extends Message
case class InitSpecs(specs: Specs) extends Message
case class Evaluate(spec: Spec) extends Message
case class Commit(spec: Spec) extends Message
case class StartTask(name: String) extends Message
case class Info(task: String, message: String) extends Message
case class Warn(task: String, message: String) extends Message
case class Error(task: String, message: String, stackTrace: String) extends Message
case class Debug(task: String, message: String) extends Message
case class Progress(task: String, progress: Int) extends Message
case class EndTask(name: String) extends Message
case class Proven(layer: String, constr: String) extends Message
case class IgnoreModel(layer: String, model: LayerObject, reason: String) extends Message
case class RemoveMappings(layer: String, model: LayerObject) extends Message
case class IssueList(issues: Set[SemanticIssue]) extends Message
case class MappingsList(mappings: Set[Mapping]) extends Message
case class AddMapping(fromLayer: String, from: LayerObject, toLayer: String, to: LayerObject) extends Message
case class Entities(layer: String, entities: Set[LayerObject]) extends Message

object Message {
  import upickle._
  
  def read(raw: String) = upickle.default.read[Message](raw)
  def write(msg: Message) = upickle.default.write(msg)
}

