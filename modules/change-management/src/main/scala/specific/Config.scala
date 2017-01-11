package specific

import com.typesafe.config._
import org.eclipse.emf.ecore.EcorePackage.Literals
import org.eclipse.emf.ecore.{EDataType, EObject, EAnnotation, EPackage}

/**
 * @author Martin Ring <martin.ring@dfki.de>
 * @since  Nov 24, 2014
 */

object Config { 
  private lazy val config = ConfigFactory.load()  
  lazy val clangCommand = config.getString("systemc.clang-command")
  lazy val systemcHome = config.getString("systemc.systemc-home")
  lazy val systemCLib = config.getString("systemc.systemc-lib")

  val ignorePackages = Set("std","sc_core","sc_dt","__gnu_cxx", "__exception_ptr", "__debug", "__gnu_debug", "__detail", "__exception_ptr")

  def filter: Any => Boolean = { x =>
    val f: Any => Boolean = {
      case pkg: EPackage =>
        !ignorePackages.contains(pkg.getName)
      case e: EAnnotation => false
      case other: EObject =>
        val nameF = other.eClass().getEStructuralFeature("name")
        nameF == null || !other.eIsSet(nameF) || {
          val name = other.eGet(nameF)
          !(name.toString.startsWith("_") || name.toString.startsWith("_"))
        }
      case other => true
    }
    f(x)
  }

  val SCTypes: PartialFunction[String,EDataType] = {
    case "void" => null
    case "int" => Literals.EINT
    case "Bool" => Literals.EBOOLEAN
  }
}