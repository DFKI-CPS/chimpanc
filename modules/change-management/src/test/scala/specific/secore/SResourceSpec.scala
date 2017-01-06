//package specific.secore
//
//import org.scalatest._
//import org.eclipse.uml2.uml.resources.util.UMLResourcesUtil
//import org.eclipse.emf.ecore.xmi.impl.EcoreResourceFactoryImpl
//import org.eclipse.papyrus.sysml.blocks.BlocksPackage
//import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
//import org.eclipse.emf.common.util.URI
//import org.dfki.stools.STools
//import java.io.File
//
//
///**
// * @author Martin Ring <martin.ring@dfki.de>
// * @since  Nov 21, 2014
// */
//class SResourceSpec extends FlatSpec with Matchers {
//  lazy val resourceSet = {
//    val rs = new ResourceSetImpl
//    UMLResourcesUtil.init(rs)
//    rs.getPackageRegistry put ("http://www.eclipse.org/papyrus/0.7.0/SysML/Blocks", BlocksPackage.eINSTANCE)
//    rs.getResourceFactoryRegistry().getExtensionToFactoryMap().put(
//      "ecore", new EcoreResourceFactoryImpl());
//    rs
//  }
//
//  val stools = new STools(new File(getClass.getResource("/ecore.simeq").getFile))
//
//  "semantic resources" should "detect differences" in {
//    import SEcore._
//    val resource1 = resourceSet.getResource(URI.createURI(this.getClass.getResource("/model.uml").getPath), true)
//    val resource2 = resourceSet.getResource(URI.createURI(this.getClass.getResource("/model1.uml").getPath), true)
//    val diff = stools.get("ecore").sdiff(new SResource(resource1), new SResource(resource2))
//    diff should not be empty
//  }
//
//  it should "detect equality" in {
//    import SEcore._
//    val resource1 = resourceSet.getResource(URI.createURI(this.getClass.getResource("/model.uml").getPath), true)
//    val resource2 = resourceSet.getResource(URI.createURI(this.getClass.getResource("/model.uml").getPath), true)
//    val diff = stools.get("ecore").sdiff(new SResource(resource1), new SResource(resource2))
//    diff shouldBe empty
//  }
//}