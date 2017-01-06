//package specific
//
//import org.scalatest._
//import specific.graph.{Cypher, ECoreToGraph, HybridResourceSet, Neo4j}
//import org.eclipse.uml2.uml.resources.util.UMLResourcesUtil
//import org.eclipse.emf.ecore.xmi.impl.EcoreResourceFactoryImpl
//import org.eclipse.papyrus.sysml.blocks.BlocksPackage
//import org.eclipse.emf.common.util.URI
//import java.io.File
//
//import de.dfki.cps.stools.STools
//import specific.secore.SEcore._
//
///**
// * @author Martin Ring <martin.ring@dfki.de>
// * @since  20.11.2014
// */
//class RoundTripTest extends FlatSpec with Matchers {
//  implicit val database = Neo4j("db")
//
//  lazy val resourceSet = {
//    val rs = new HybridResourceSet
//    UMLResourcesUtil.init(rs)
//    rs.getPackageRegistry put ("http://www.eclipse.org/papyrus/0.7.0/SysML/Blocks", BlocksPackage.eINSTANCE)
//    rs.getResourceFactoryRegistry().getExtensionToFactoryMap().put(
//      "ecore", new EcoreResourceFactoryImpl());
//    rs
//  }
//
//  def loadResourceFromFile(filename: String) =
//    resourceSet.getResource(URI.createFileURI(filename), true)
//
//  def loadResourceFromGraph(rootId: String) = {
//    val uri = URI.createURI(s"graph://$rootId")
//    resourceSet.getResource(uri, true)
//  }
//
//  def clearDB = {
//    val query = "START n = node(*) OPTIONAL MATCH n-[r]-() WITH n, r LIMIT 10000 DELETE n, r"
//    Cypher(query)
//  }
//
//  val stools = new STools(new File(getClass.getResource("/ecore.simeq").getFile))
//
//  "A graph resource in a hybrid resouce set" should "be equal to the origin xml resource" in {
//    clearDB()
//    val xmlResource = loadResourceFromFile(getClass.getResource("/model.uml").getPath)
//    // write resource to graph
//    ECoreToGraph.writeResource(xmlResource, "roundTrip")
//    // read resource from graph
//    val graphResource = loadResourceFromGraph("roundTrip")
//    val diff = stools.get("ecore").sdiff(new SResource(xmlResource), new SResource(graphResource))
//    assert(diff.isEmpty)
//  }
//}