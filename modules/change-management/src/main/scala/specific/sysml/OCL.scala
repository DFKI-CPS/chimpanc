package specific.sysml

import java.io.{File, FileInputStream}

import org.eclipse.emf.common.util.{BasicEList, EList}
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.emf.ecore.{EObject, EOperation, EClass}
import org.eclipse.ocl.{OCLInput}
import org.eclipse.ocl.{ ecore => ocl }
import org.eclipse.ocl.ecore.{OperationCallExp, Constraint, EcoreEnvironmentFactory}
import specific._
import specific.graph.{GraphResource, GraphEObject, GraphEList, ECoreToGraph}
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import scala.util.Try

/**
 * Created by martin on 31.07.15.
 */
sealed trait OCLExpression

sealed trait CallExp extends OCLExpression {
  val source: OCLExpression
  //val x: OperationCallExp
}

case class FeatureCallExp(source: OCLExpression,
                          isPre: Boolean) extends CallExp

sealed trait LoopExp extends CallExp

case class IteratorExp(source: OCLExpression) extends LoopExp
case class IterateExp(source: OCLExpression) extends LoopExp

case class LiteralExp() extends OCLExpression

case class IfExp(condition: OCLExpression,
                 thenExpression: OCLExpression,
                 elseExpression: OCLExpression) extends OCLExpression

case class VariableExp(initExpression: Option[OCLExpression],
                       representedParameter: OCLExpression) extends OCLExpression
case class TypeExp() extends OCLExpression
case class MessageExp() extends OCLExpression
case class StateExp() extends OCLExpression

object OCL {
  def getInfos(resource: Seq[Constraint]): Seq[ConstraintInfo] = {
    resource.map { c =>
      c.getStereotype match {
        case "invariant" =>
          Invariant(Clazz(c.getConstrainedElements.head.asInstanceOf[EClass].getName), c.getName)
        case "precondition" =>
          val op = c.getConstrainedElements.head.asInstanceOf[EOperation]
          val clazz = op.getEContainingClass.getName
          val name = op.getName
          val tpe = Option(op.getEType).map(_.getName).getOrElse("void")
          val params = op.getEParameters.toSeq.map(p => (p.getName, p.getEType.getName))
          Precondition(Operation(clazz,name,tpe,params), c.getName)
        case "postcondition" =>
          val op = c.getConstrainedElements.head.asInstanceOf[EOperation]
          val clazz = op.getEContainingClass.getName
          val name = op.getName
          val tpe = Option(op.getEType).map(_.getName).getOrElse("void")
          val params = op.getEParameters.toSeq.map(p => (p.getName, p.getEType.getName))
          Postcondition(Operation(clazz,name,tpe,params), c.getName)
      }
    }
  }

  def load(filename: String, resource: Resource)(implicit resourceSet: ResourceSet): Try[Seq[Constraint]] = {
    require(new File(filename).exists(), s"file '$filename' not found")
    val ocl = org.eclipse.ocl.ecore.OCL.newInstance(new EcoreEnvironmentFactory(resourceSet.getPackageRegistry), resource)
    val in2 = new String
    val in = new FileInputStream(filename)
    val o = new OCLInput(in)
    val res = Try(ocl.parse(o))
    res.map { constraints =>
      if (constraints.exists(_.getName == null)) sys.error(s"Unnamed constraints are not supported.")
      constraints
    }
  }
}