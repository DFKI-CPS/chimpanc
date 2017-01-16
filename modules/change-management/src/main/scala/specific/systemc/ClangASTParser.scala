package specific.systemc

import java.io.File

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore._
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import org.eclipse.emf.ecore.resource.{Resource, ResourceSet}
import org.eclipse.emf.ecore.xmi.impl.{EcoreResourceFactoryImpl, URIHandlerImpl}
import org.eclipse.emf.ecore.xmi.XMLResource
import specific.emf.cpp._
import specific.{ChangeManagement, Config}
import specific.util.Helpers._

import scala.collection.mutable
import scala.sys.process._
import scala.collection.JavaConversions._
import scala.util.Try

object ClangASTParser {

  trait Handler extends PartialFunction[(String, String), Option[Handler]]

  def timed[T](action: String)(f: => T): T = {
    val t0 = System.currentTimeMillis()
    val result = f
    val d = System.currentTimeMillis() - t0
    println(s"$action took $d ms")
    result
  }

  def handle(f: PartialFunction[(String, String), Option[Handler]]): Option[Handler] = Some(new Handler {
    override def isDefinedAt(x: (String, String)): Boolean = f.isDefinedAt(x)
    override def apply(v1: (String, String)): Option[Handler] = f.apply(v1)
  })

  var nodes = 0

  def read[T](filename: String)(h: PartialFunction[(String, String), Option[Handler]]) = {
    val stdlib = if (System.getProperty("os.name").toLowerCase.contains("mac")) "-stdlib=libc++" else "-stdlib=libstdc++"
    val clangProcess = Seq(
      Config.clangCommand,
      "-Xclang",
      "-ast-dump",
      "-fsyntax-only",
      "-std=c++11", stdlib,
      s"-I${Config.systemcHome}include/"
    ) :+ filename

    var handler = mutable.Stack(h)
    def indent = handler.size - 1

    clangProcess.lineStream.foreach { line =>
      nodes += 1
      val (i, content) = line.span(!_.isLetterOrDigit)
      val ind = i.length / 2
      if (ind < indent)
        for (x <- 0 until (indent - ind)) handler.pop()
      if (ind == indent) {
        val (name, rest) = content.span(_.isLetterOrDigit)
        Try(handler.top.isDefinedAt((name, rest.tail))).map { if (_) {
          //if (ind < 5) println(line)
          //if (line.contains(" size_type ")) println("####: " + line)
          val res = handler.top((name, rest.tail))
          res.foreach(handler.push(_))
        } }.failed.foreach { x =>
          println("CLANG ERROR: " + x.getMessage)
          println(content)
          println(name)
          println(rest)
        }
      }
    }
  }

  val Template = ".* ([a-zA-Z_][a-zA-Z0-9_]*)".r
  val TemplateTypeParam = ".* (?:used|referenced) (?:class|struct|typename) ([a-zA-Z_][a-zA-Z0-9_]*)".r
  val Visibility = ".* (default|private|public)".r
  val Class = ".* (?:class|struct|typename) ([a-zA-Z_][a-zA-Z0-9_]*) definition".r
  val Enum = ".* (?:referenced |used )?([a-zA-Z_][a-zA-Z0-9_]*)".r
  val EnumLiteral = ".* (?:referenced |used )?([a-zA-Z_][a-zA-Z0-9_]*) '.*'".r
  val Namespace = ".* ([a-zA-Z][a-zA-Z0-9_]*)".r
  val Field = ".*(?: used| referenced)? ([a-zA-Z_][a-zA-Z0-9_]*) ('.*')( static|)(?: mutable)?(?: cinit)?".r
  val Method = ".*(?: used| referenced)? ([a-zA-Z_][a-zA-Z0-9_]*) ('.* )\\(.*\\)'(?: const| noexcept)*( static|)".r
  val Parameter = ".*(?: used| referenced)? ([a-zA-Z_][a-zA-Z0-9_]*) ('.*')".r

  val TypedefDecl = ".*(?:used|referenced)? ([a-zA-Z_][a-zA-Z0-9_]*) ('.*')".r
  //val GenericType = "[^']*(?:'[^']+':)?'(?:class|struct) ([a-zA-Z_][a-zA-Z0-9_]*)::([a-zA-Z][a-zA-Z0-9_]*)<(.*)>'".r
  val Type = "[^']*(?:'[^']+':)?'(?:class|struct) ([a-zA-Z_][a-zA-Z0-9_]*)::([a-zA-Z][a-zA-Z0-9_]*)(?:<.*>)?'".r

  val Array = "(.*)\\[([0-9]+)\\]".r
  val Void = ".*'void \\(.*\\)'.*".r
  val Const = "const (.*)".r
  val Clazz = "class (.*)".r
  val Struct = "struct (.*)".r
  val EnumT = "enum (.*)".r
  val Typename = "typename (.*)".r
  val Pointer = "(.*)\\*".r
  val Ref = "(.*)&".r

  val LocalClassName = "([a-zA-Z_][a-zA-Z0-9_]*)".r
  val LocalGenericClassName = "([a-zA-Z_][a-zA-Z0-9_]*)<(.*)>".r
  val PackageAndClassName = "([a-zA-Z_][a-zA-Z0-9_]*)((?:::[a-zA-Z_][a-zA-Z0-9_]*)+(?:<.*>)?)".r

  //val libs = resourceSet.createResource(URI.createFileURI(filename + ".lib.ecore"))

  def parse(filename: String)(implicit resourceSet: ResourceSet): Try[Resource] = Try {
    val eFactory = EcoreFactory.eINSTANCE
    val factory = CppFactory.eINSTANCE
    val libs = mutable.Map.empty[String, Resource]

    def libraryNamespace(uri: String, pkgName: String): CppNamespace = {
      val lib = resourceSet.createResource(URI.createFileURI(URI.decode(filename + s".lib.${pkgName}.ecore")))
      libs += uri -> lib
      val pkg = factory.createCppNamespace()
      pkg.setNsURI(uri)
      resourceSet.getPackageRegistry.put(uri, pkg)
      pkg.setName(pkgName)
      lib.getContents.add(pkg)
      pkg
    }

    val cpp = libraryNamespace("http://www.dfki.de/SPECifIC/cpp", "cpp")

    def cppPrimitive(name: String) = {
      val tpe = eFactory.createEDataType()
      tpe.setName(name)
      cpp.getEClassifiers.add(tpe)
      tpe
    }

    def typedef(name: String, to: Either[EClassifier, EGenericType]): CppTypedef = {
      val tdef = factory.createCppTypedef()
      tdef.setName(name)
      to match {
        case Left(c) =>
          tdef.setEClassifier(c)
        case Right(x) =>
          eFactory.getEcorePackage.getEGenericType.getEStructuralFeatures
            .filter(x.eIsSet)
            .filter(_.isChangeable)
            .foreach { f =>
            tdef.eSet(f, x.eGet(f))
          }
      }
      tdef
    }

    object cppPrimitives {
      val void = cppPrimitive("void")

      val bool = cppPrimitive("bool")

      val schar = cppPrimitive("signed char")
      val uchar = cppPrimitive("unsigned char")
      val char = cppPrimitive("char")
      val wchar_t = cppPrimitive("wchar_t")
      val char16_t = cppPrimitive("char16_t")
      val char32_t = cppPrimitive("char32_t")

      val short = cppPrimitive("short int")
      val ushort = cppPrimitive("unsigned short int")

      val int = cppPrimitive("int")
      val uint = cppPrimitive("unsigned int")

      val long = cppPrimitive("long int")
      val ulong = cppPrimitive("unsigned long int")

      val longLong = cppPrimitive("long long int")
      val ulongLong = cppPrimitive("unsigned long long int")

      val float = cppPrimitive("float")
      val double = cppPrimitive("double")
      val ldouble = cppPrimitive("long double")
    }

    val libraryNamespaces = Map[String, CppNamespace](
      "cpp" -> cpp,
      "std" -> libraryNamespace("http://www.dfki.de/SPECifIC/cpp/std", "std"),
      "sc_core" -> libraryNamespace("http://www.dfki.de/SPECifIC/systemc/sc_core", "sc_core"),
      "sc_boost" -> libraryNamespace("http://www.dfki.de/SPECifIC/systemc/sc_boost", "sc_boost"),
      "sc_dt" -> libraryNamespace("http://www.dfki.de/SPECifIC/systemc/sc_dt", "sc_dt")
    )

    val resource = resourceSet.createResource(URI.createFileURI(filename + ".ecore"))


    def tlqName(p: EPackage): String =
      Option(p.getESuperPackage).map(tlqName).fold(p.getName)(_ + "::" + p.getName)

    def qName(c: CppClass): String =
      Option(c.getParentClass).map(qName).orElse(
        Option(c.getEPackage).map(tlqName)).fold(c.getName)(_ + "::" + c.getName)

    object Scope {
      def apply(r: Resource): Scope = new Scope {
        override def get(path: String): Option[EObject] = {
          r.getContents.find {
            case c: CppNamespace if c.getName == path => true
            case c: CppClass if c.getName == path => true
            case _ => false
          } orElse
            libs.flatMap(_._2.getContents).find {
              case p: CppNamespace if p.getName == path => true
              case c: CppClass if c.getName == path => true
              case _ => false
            }
        } orElse libraryNamespaces.values.flatMap(_.getTypedefs).find(_.getName == path) orElse
          libraryNamespaces.values.flatMap(_.getEClassifiers).find(_.getName == path)

        override def context: String = "<root>"

        override def withTypedefs[T](t: => T) = t
      }

      def apply(p: EPackage): Scope = new Scope {
        override def get(path: String): Option[EObject] = {
          p.asOpt[CppNamespace].flatMap(_.getTypedefs.find(_.getName == path)) orElse
            p.getEClassifiers.find(_.getName == path) orElse
            p.getESubpackages.find(_.getName == path) orElse
            Option(p.getESuperPackage).flatMap(p => Scope(p).get(path)) orElse
            Option(p.eResource).flatMap(x => Scope(x).get(path))
        }

        override def context: String = tlqName(p)

        override def withTypedefs[T](t: => T) = {
          val before = p.asOpt[CppNamespace].toList.flatMap(_.getTypedefs)
          val result = t
          assert(before == p.asOpt[CppNamespace].toList.flatMap(_.getTypedefs), s"typedefs changed in $context")
          result
        }
      }

      def apply(c: CppClass): Scope = new Scope {
        override def get(path: String): Option[EObject] = {
          c.getTypedefs.find(_.getName == path) orElse
            c.getETypeParameters.find(_.getName == path) orElse
            c.getNestedClasses.find(_.getName == path) orElse
            c.getEAllSuperTypes.collect { case c: CppClass => c }.flatMap(_.getNestedClasses).find(_.getName == path) orElse
            c.getEAllSuperTypes.collect { case c: CppClass => c }.flatMap(_.getTypedefs).find(_.getName == path) orElse
            c.getEAllSuperTypes.collect { case c: CppClass => c }.flatMap(_.getETypeParameters).find(_.getName == path) orElse
            Option(c.getParentClass).flatMap(c => Scope(c).get(path)) orElse
            Option(c.getEPackage).flatMap(p => Scope(p).get(path)) orElse
            Option(c.eResource()).flatMap(r => Scope(r).get(path))
        }

        override def context: String = qName(c)

        override def withTypedefs[T](t: => T) = {
          val before = c.getNamespace.getTypedefs.toList
          val result = t
          assert(before == c.getNamespace.getTypedefs.toList, s"typedefs changed in $context")
          result
        }
      }
    }

    def findTLPackage(resource: Resource, name: String): Option[CppNamespace] = libraryNamespaces.get(name) orElse resource.getContents.collectFirst {
      case p: CppNamespace if p.getName == name => p
    }

    def findPackage(parent: EPackage, name: String): Option[CppNamespace] = parent.getESubpackages.collectFirst {
      case p: CppNamespace if p.getName == name => p
    }

    def getTLPackage(resource: Resource, name: String): CppNamespace = {
      findTLPackage(resource, name).getOrElse {
        val pkg = factory.createCppNamespace()
        pkg.setName(name)
        resource.getContents.add(pkg)
        pkg
      }
    }

    def getPackage(parent: CppNamespace, name: String): CppNamespace = {
      findPackage(parent, name).getOrElse {
        val pkg = factory.createCppNamespace()
        pkg.setName(name)
        parent.getESubpackages.add(pkg)
        pkg
      }
    }

    def findTLClass(pkg: EPackage, name: String): Option[CppClass] = {
      pkg.getEClassifiers.collectFirst { case c: CppClass if c.getName == name => c }
    }

    def findEnum(pkg: EPackage, name: String): Option[EEnum] = {
      Option(pkg.getEClassifier(name)).filter(_.isInstanceOf[EEnum]).map(_.asInstanceOf[EEnum])
    }

    def findClass(parent: CppClass, name: String): Option[CppClass] = {
      parent.getNestedClasses.find(_.getName == name)
    }

    def getClass(parent: CppClass, name: String): CppClass = {
      findClass(parent, name).getOrElse {
        val c = factory.createCppClass()
        c.setName(name)
        parent.getNestedClasses.add(c)
        c
      }
    }

    def getTLClass(pkg: EPackage, name: String): CppClass = {
      findTLClass(pkg, name).getOrElse {
        val c = factory.createCppClass()
        c.setName(name)
        pkg.getEClassifiers.add(c)
        c
      }
    }

    def getEnum(pkg: EPackage, name: String): EEnum = {
      findEnum(pkg, name).getOrElse {
        val c = eFactory.createEEnum()
        c.setName(name)
        pkg.getEClassifiers.add(c)
        c
      }
    }

    def resolveClass(tpe: String): Option[CppClass] = {
      val result: Option[CppClass] = tpe match {
        case Type(pkg, name) =>
          for {
            p <- findTLPackage(resource, pkg)
            c <- findTLClass(p, name)
          } yield c
        case _ => None
      }
      result
    }

    def genericType(c: EClassifier): EGenericType = {
      val gt = eFactory.createEGenericType()
      gt.setEClassifier(c)
      gt
    }
    
    var unresolved = 0
    def resolveTypeName(scope: Scope, tname: String, originalScope: Option[Scope] = None): Option[Either[EClassifier, EGenericType]] = {
      //println(s"LOOKING FOR $tname in ${pkge.getName} and ${clazz.map(_.getName)}")
      tname match {
        case Pointer(tpe) => resolveTypeName(scope, tpe.trim, originalScope)
        case Ref(tpe) => resolveTypeName(scope, tpe.trim, originalScope)
        case Array(tpe, mult) => resolveTypeName(scope, tpe.trim, originalScope)
        case Clazz(tpe) => resolveTypeName(scope, tpe.trim, originalScope)
        case Struct(tpe) => resolveTypeName(scope, tpe.trim, originalScope)
        case EnumT(tpe) => resolveTypeName(scope, tpe.trim, originalScope)
        case Typename(tpe) => resolveTypeName(scope, tpe.trim, originalScope)
        case Const(tpe) => resolveTypeName(scope, tpe.trim, originalScope)
        case "void" => Some(Left(cppPrimitives.void))
        case "bool" | "_Bool" => Some(Left(cppPrimitives.bool))
        case "signed char" => Some(Left(cppPrimitives.schar))
        case "unsigned char" => Some(Left(cppPrimitives.uchar))
        case "char" => Some(Left(cppPrimitives.char))
        case "wchar_t" => Some(Left(cppPrimitives.wchar_t))
        case "char16_t" => Some(Left(cppPrimitives.char16_t))
        case "char32_t" => Some(Left(cppPrimitives.char32_t))
        case "short" | "short int" | "signed short" | "signed short int" => Some(Left(cppPrimitives.short))
        case "unsigned short" | "unsigned short int" => Some(Left(cppPrimitives.ushort))
        case "int" | "signed" | "signed int" => Some(Left(cppPrimitives.int))
        case "unsigned" | "unsigned int" => Some(Left(cppPrimitives.uint))
        case "long" | "long int" | "signed long" | "signed long int" => Some(Left(cppPrimitives.long))
        case "unsigned long" | "unsigned long int" => Some(Left(cppPrimitives.ulong))
        case "long long" | "long long int" | "signed long long" | "signed long long int" => Some(Left(cppPrimitives.longLong))
        case "unsigned long long" | "unsigned long long int" => Some(Left(cppPrimitives.ulongLong))
        case "float" => Some(Left(cppPrimitives.float))
        case "double" => Some(Left(cppPrimitives.double))
        case "long double" => Some(Left(cppPrimitives.ldouble))
        case LocalClassName(name) =>
          scope.get(name).collect {
            case c: EClassifier => Left(c)
            case td: CppTypedef => Right(td)
            case g: EGenericType => Right(g)
            case t: ETypeParameter =>
              val g = eFactory.createEGenericType()
              g.setETypeParameter(t)
              Right(g)
          }
        case LocalGenericClassName(name, args) =>
          resolveTypeName(scope, name, originalScope).collect {
            case Left(c) =>
              val gt = genericType(c)
              args.split(",").map(_.trim).map(resolveTypeName(originalScope.getOrElse(scope), _)).foreach {
                case Some(Left(c)) => gt.getETypeArguments.add(genericType(c))
                case Some(Right(g: CppTypedef)) =>
                  val ref = factory.createCppTypedefRef()
                  ref.setTypedef(g)
                  gt.getETypeArguments.add(ref)
                case Some(Right(g)) => gt.getETypeArguments.add(g)
                case None => //println("WARN: could not resolve type argument " + tname)
              }
              Right(gt)
          }
        case PackageAndClassName(pkg, name) =>
          scope.get(pkg).collect {
            case c: CppTypedef if c.getEClassifier != null && c.getEClassifier.isInstanceOf[CppClass] =>
              resolveTypeName(Scope(c.getEClassifier.asInstanceOf[CppClass]), name.drop(2), Some(scope))
            case c: CppClass => resolveTypeName(Scope(c), name.drop(2), Some(scope))
            case p: CppNamespace => resolveTypeName(Scope(p), name.drop(2), Some(scope))
            //case x: ETypeParameter if println("TRYING TO RESOLVE " + x.getName + name + " IN " + scope.context) == true => None
          }.flatten
        case other => None
      }
    }


    def resolve(scope: Scope, tpe: String): Option[Either[EClassifier, EGenericType]] = {
      val ts = tpe.substring(1, tpe.length - 1).split("':'").map(_.trim)
      val x = ts.collectFirst {
        case x if resolveTypeName(scope, x).isDefined =>
          resolveTypeName(scope, x).get
      }
      if (x.isEmpty && !ts.exists(_.contains("void"))) {
        unresolved += 1
        //println("could not resolve " + tpe + " in " + scope.context)
        //scope.take(10).foreach(println)
      }
      x
    }

    trait Scope {
      def get(path: String): Option[EObject]

      def context: String

      def withTypedefs[T](t: => T): T
    }

    def setType(e: EObject, t: Option[Either[EClassifier, EGenericType]]) = t.foreach(t => (e, t) match {
      case (e: EOperation, Left(t)) => e.setEType(t)
      case (e: CppOperation, Right(t: CppTypedef)) => e.setTypedef(t)
      case (e: EAttribute, Left(t)) => e.setEType(t)
      case (e: CppAttribute, Right(t: CppTypedef)) => e.setTypedef(t)
      case (e: EParameter, Left(t)) => e.setEType(t)
      case (e: CppParameter, Right(t: CppTypedef)) => e.setTypedef(t)
      case (e, Right(t: CppTypedef)) => sys.error("FAAAAAAIL " + e)
      case (e: EAttribute, Right(t)) => e.setEGenericType(t)
      case (e: EOperation, Right(t)) => e.setEGenericType(t)
      case (e: EParameter, Right(t)) => e.setEGenericType(t)
    })

    def handlePackage(pkg: CppNamespace): Option[Handler] = handle {
      case ("NamespaceDecl", Namespace(name)) =>
        val subpkg = getPackage(pkg, name)
        handlePackage(subpkg)
      case ("ClassTemplateDecl", Template(name)) =>
        val clazz: CppClass = getTLClass(pkg, name)
        handle {
          case ("TemplateTypeParmDecl", TemplateTypeParam(name)) =>
            val param = eFactory.createETypeParameter()
            param.setName(name)
            clazz.getETypeParameters.add(param)
            /*handle {
                case ("TemplateArgument", x) if x.startsWith("type") =>
                  clazz.getETypeParameters.remove(param)
                  None
              }*/
            None
          case ("CXXRecordDecl", Class(cname)) if name == cname =>
            handleClass(clazz)
        }
      case ("FunctionDecl", Method(name, tpe, _)) =>
        val op = factory.createCppOperation()
        op.setName(name)
        op.setStatic(true)
        pkg.getOperations.add(op)
        setType(op, resolve(Scope(pkg), tpe))
        handle {
          case ("ParmVarDecl", Parameter(name, tpe)) =>
            val parm = factory.createCppParameter()
            parm.setName(name)
            op.getEParameters.add(parm)
            setType(parm, resolve(Scope(pkg), tpe))
            None
        }
      case ("TypedefDecl" | "TypeAliasDecl", TypedefDecl(name, tpe)) =>
        resolve(Scope(pkg), tpe).foreach { x =>
          val tdef = typedef(name, x)
          pkg.getTypedefs.add(tdef)
        }
        None
      case ("EnumDecl", Enum(name)) =>
        val enum: EEnum = getEnum(pkg, name)
        handle {
          case ("EnumConstantDecl", EnumLiteral(name)) =>
            val lit = eFactory.createEEnumLiteral()
            lit.setName(name)
            enum.getELiterals.add(lit)
            None
        }
      case ("CXXRecordDecl", Class(name)) =>
        handleClass(getTLClass(pkg, name))
    }

    def handleClass(clazz: CppClass): Option[Handler] = {
      var visibility: CppVisibility = CppVisibility.DEFAULT
      handle {
        case ("public", tpe) =>
          resolveClass(tpe).foreach(clazz.getESuperTypes.add)
          None
        case ("FieldDecl" | "VarDecl", Field(name, tpe, static)) =>
          val f = factory.createCppAttribute()
          f.setName(name)
          f.setStatic(static.nonEmpty)
          f.setVisibility(visibility)
          clazz.getEStructuralFeatures.add(f)
          val res = resolve(Scope(clazz), tpe)
          setType(f, res)
          None
        case ("CXXMethodDecl", Method(name, tpe, static)) =>
          val op = factory.createCppOperation()
          op.setName(name)
          op.setStatic(static.nonEmpty)
          op.setVisibility(visibility)
          clazz.getEOperations.add(op)
          val x = resolve(Scope(clazz), tpe)
          setType(op, x)
          handle {
            case ("ParmVarDecl", Parameter(name, tpe)) =>
              val parm = factory.createCppParameter()
              parm.setName(name)
              op.getEParameters.add(parm)
              setType(parm, resolve(Scope(clazz), tpe))
              None
          }
        case ("TypedefDecl" | "TypeAliasDecl", TypedefDecl(name, tpe)) =>
          resolve(Scope(clazz), tpe).foreach { x =>
            val tdef = typedef(name, x)
            tdef.setVisibility(visibility)
            clazz.getTypedefs.add(tdef)
          }
          None
        case ("CXXRecordDecl", Class(name)) =>
          val c = getClass(clazz, name)
          c.setVisibility(visibility)
          handleClass(c)
        case ("AccessSpecDecl", Visibility("default")) =>
          visibility = CppVisibility.DEFAULT
          None
        case ("AccessSpecDecl", Visibility("private")) =>
          visibility = CppVisibility.PRIVATE
          None
        case ("AccessSpecDecl", Visibility("public")) =>
          visibility = CppVisibility.PUBLIC
          None
      }
    }

    def handleTopLevel: Option[Handler] = handle {
      case ("LinkageSpecDecl", _) =>
        handleTopLevel
      case ("NamespaceDecl", Namespace(name)) if !name.startsWith("_") =>
        val pkg = getTLPackage(resource, name)
        handlePackage(pkg)
      case ("CXXRecordDecl", Class(name)) =>
        handleClass(getTLClass(getTLPackage(resource,"<root>"), name))
      case ("ClassTemplateDecl", Template(name)) =>
        val clazz: CppClass = getTLClass(getTLPackage(resource,"<root>"), name)
        handle {
          case ("TemplateTypeParmDecl", TemplateTypeParam(name)) =>
            val param = eFactory.createETypeParameter()
            param.setName(name)
            clazz.getETypeParameters.add(param)
            /*handle {
                case ("TemplateArgument", x) if x.startsWith("type") =>
                  clazz.getETypeParameters.remove(param)
                  None
              }*/
            None
          case ("CXXRecordDecl", Class(cname)) if name == cname =>
            handleClass(clazz)
        }
      case ("TypedefDecl" | "TypeAliasDecl", TypedefDecl(name, tpe)) =>
        resolve(Scope(getTLPackage(resource,"<root>")), tpe).foreach { x =>
          val tdef = typedef(name, x)
          getTLPackage(resource,"<root>").getTypedefs.add(tdef)
        }
        None
      case ("EnumDecl", Enum(name)) =>
        val enum: EEnum = getEnum(getTLPackage(resource,"<root>"), name)
        handle {
          case ("EnumConstantDecl", EnumLiteral(name)) =>
            val lit = eFactory.createEEnumLiteral()
            lit.setName(name)
            enum.getELiterals.add(lit)
            None
        }
    }

    timed("parse") {
      read(filename) {
        case ("TranslationUnitDecl", _) => handleTopLevel
      }
    }

    println(s"processed $nodes nodes")
    nodes = 0
    println(unresolved + " unresolved types")

    val uriHandler = new URIHandlerImpl {
      override def deresolve(uri: URI): URI =
        if (uri.isFile && uri.path() == baseURI.path()) // local
          URI.createURI("#" + uri.fragment())
        else if (uri.isFile)
          libs.collectFirst { case (u, res) if res.getURI.path() == uri.path() => URI.createURI(u + "#" + uri.fragment()) }.get
        else super.deresolve(uri)
    }

    val options = Map(XMLResource.OPTION_URI_HANDLER -> uriHandler)

    timed("save") {
      libs.values.foreach(_.save(options))
      resource.save(options)
    }

    resource
  }
}