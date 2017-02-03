package specific

/**
 * @author martin
 */

sealed trait SemanticIssue
case class RequirementEvaluation(layer: String, path: String, results: Seq[Option[Boolean]]) extends SemanticIssue
case class UnimplementedModel(layer: String, model: LayerObject) extends SemanticIssue
case class MaltypedMapping(layer: String, model: LayerObject, expected: String, actual: String) extends SemanticIssue
case class IgnoredModel(layer: String, model: LayerObject, reason: String) extends SemanticIssue
case class RemovedModel(layer: String, model: LayerObject, otherLayer: String, implementation: LayerObject) extends SemanticIssue
case class RemovedImplementation(layer: String, model: LayerObject, otherLayer: String, implementation: LayerObject) extends SemanticIssue
case class ModifiedImplementation(layer: String, model: LayerObject, otherLayer: String, implementation: LayerObject) extends SemanticIssue
case class MismatchingBounds(layer: String, model: LayerObject, expected: String, actual: String) extends SemanticIssue
case class OCLProofObligation(layer: String, owner: String, implication: String, proven: Boolean, cLine: Int, cColumn: Int) extends SemanticIssue

sealed trait Spec { val name: String }
case class SysML(name: String, content: String) extends Spec
case class Specs(layers: Seq[Spec])

case class LayerObject(path: String, name: String, line: Int, column: Int)

sealed trait SemanticInfo
case class Mapping(fromLayer: String, from: String, toLayer: String, to: String) extends SemanticInfo

sealed trait Message
case class Init(nlp: String, fsl: String, esl: String) extends Message
case class InitSpecs(specs: Specs) extends Message
case class Update(specs: Specs) extends Message
case class Evaluate(spec: Spec) extends Message
case object Commit extends Message
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

case object CommitAll extends Message

object Message {
  import upickle._
  
  def read(raw: String) = upickle.default.read[Message](raw)
  def write(msg: Message) = upickle.default.write(msg)
}

