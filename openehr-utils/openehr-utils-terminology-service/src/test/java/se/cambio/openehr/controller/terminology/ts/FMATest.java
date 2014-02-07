package se.cambio.openehr.controller.terminology.ts;


public class FMATest extends TerminologyServiceTestBase {
	private static final String FMA = "FMA"; 
	public FMATest() {
		super();
		// TODO Auto-generated constructor stub
	}
	
	public void testDummy(){
	   // assertTrue(true);
	}
	/*TODO Ontology support:
	public void testFMASupported() {
		assertTrue(ts.isTerminologySupported(FMA));
	}
	
	public void testCheckDirectSubclassExpectedTrue() throws Exception {
		CodePhrase spleen = new CodePhrase(FMA, "7196");
		CodePhrase parenchymatousOrgan = new CodePhrase(FMA, "55663");
		assertTrue(ts.isSubclassOf(spleen,  parenchymatousOrgan));
	}
	
	public void testCheckDirectSubclassExpectedFalse() throws Exception {
		CodePhrase spleen = new CodePhrase(FMA, "7196");
		CodePhrase kidney = new CodePhrase(FMA, "7203");
		assertFalse(ts.isSubclassOf(spleen,  kidney));
	}
	
	public void testCheckIndirectSubclassExpectedTrue() throws Exception {
		CodePhrase spleen = new CodePhrase(FMA, "7196");
		CodePhrase solidOrgan = new CodePhrase(FMA, "55670");
		assertTrue(ts.isSubclassOf(spleen,  solidOrgan));
	}
	
	public void testCheckIndirectSubclassExpectedFalse() throws Exception {
		CodePhrase spleen = new CodePhrase(FMA, "7196");
		CodePhrase lobularOrgan = new CodePhrase(FMA, "55662");
		assertFalse(ts.isSubclassOf(spleen,  lobularOrgan));
	}
	
	public void testRetrieveAllSubclasses() throws Exception {
		CodePhrase kidney = new CodePhrase(FMA, "7203");
		TerminologyNodeVO tree = ts.retrieveAllSubclasses(kidney, EN);
		assertEquals(2, tree.getChildren().size()); // left & right
	}
	*/

}
