package se.cambio.openehr.controller.terminology.ts;

public class SNOMEDCTTest extends TerminologyServiceTestBase {
	
	public SNOMEDCTTest() throws Exception {
		super();
		// TODO Auto-generated constructor stub
	}

	public void testDummy(){
	    assertTrue(true);
	}
	// heart failure/chronic heart failure
	/*TODO Ontology support:
	public void testCheckSubclassWithDirectSubclass() throws Exception {		
		CodePhrase chronicHeartFailure = new CodePhrase(SCT, "48447003");
		CodePhrase heartFailure = new CodePhrase(SCT, "84114007");
		boolean ret = ts.isSubclassOf(chronicHeartFailure, heartFailure);
		assertTrue("expected true", ret);
	}
	
	public void testCheckSubclassExpectedFalse() throws Exception {		
		CodePhrase malignantLymphoma = new CodePhrase(SCT, "118600007");
		CodePhrase heartFailure = new CodePhrase(SCT, "84114007");
		boolean ret = ts.isSubclassOf(malignantLymphoma, heartFailure);
		assertFalse("expected false", ret);
	}
	
	// Heart failure/Acute heart failure/active congestive heart failure
	public void testCheckSubclassWithDescendantClass() throws Exception {		
		CodePhrase actueCongestiveHeartFailure = new CodePhrase(SCT, "10633002");
		CodePhrase heartFailure = new CodePhrase(SCT, "84114007");
		boolean ret = ts.isSubclassOf(actueCongestiveHeartFailure, heartFailure);
		assertTrue("expected true", ret);
	}
	
	public void testCheckSubClassWithSetExpectTrue() throws Exception {
		CodePhrase actueCongestiveHeartFailure = new CodePhrase(SCT, "10633002");
		CodePhrase heartFailure = new CodePhrase(SCT, "84114007");
		CodePhrase malignantLymphoma = new CodePhrase(SCT, "118600007");
		CodePhrase chronicalHeartFailure = new CodePhrase(SCT, "206596003");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(heartFailure);
		codes.add(malignantLymphoma);
		codes.add(chronicalHeartFailure);
		boolean ret = ts.isSubclassOf(actueCongestiveHeartFailure, codes);
		assertTrue("expected true", ret);
	}
	
	public void testCheckSubClassWithSetExpectFalse() throws Exception {
		CodePhrase actueCongestiveHeartFailure = new CodePhrase(SCT, "10633002");
		CodePhrase heartFailure = new CodePhrase(SCT, "84114007");
		CodePhrase malignantLymphoma = new CodePhrase(SCT, "118600007");
		CodePhrase chronicalHeartFailure = new CodePhrase(SCT, "206596003");
		Set<CodePhrase> codes = new HashSet<CodePhrase>();
		codes.add(heartFailure);
		codes.add(actueCongestiveHeartFailure);
		codes.add(chronicalHeartFailure);
		boolean ret = ts.isSubclassOf(malignantLymphoma, codes);
		assertFalse("expected false", ret);
	}
	
	public void testRetrieveAllSubclassesWithNestedThree() throws Exception {
		CodePhrase code = new CodePhrase(SCT, "128292002");
		TerminologyNodeVO node = ts.retrieveAllSubclasses(code, EN);
		assertEquals(new DvCodedText("Chronic disease of cardiovascular system (disorder)", code), node.getValue());
		assertEquals(3, node.getChildren().size());
		
		boolean chronicHeartDiseaseFound = false;
		boolean chronicHypertensionInObstetricContext = false; 
		for(TerminologyNodeVO child : node.getChildren()) {
			if(child.getValue().getValue().equals("Chronic heart disease (disorder)")) {
				chronicHeartDiseaseFound = true;
				assertEquals(1, child.getChildren().size());
				assertEquals(1, child.getChildren().get(0).getChildren().size());
			}
			if(child.getValue().getValue().equals("Chronic hypertension in obstetric context (disorder)")) {
				chronicHypertensionInObstetricContext = true;
				assertEquals(3, child.getChildren().size());			
			}
		}
		assertTrue(chronicHeartDiseaseFound);
		assertTrue(chronicHypertensionInObstetricContext);
	}
	*/
}
