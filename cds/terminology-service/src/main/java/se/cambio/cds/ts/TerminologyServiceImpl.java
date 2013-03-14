package se.cambio.cds.ts;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.semanticweb.HermiT.Reasoner.ReasonerFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;

import se.cambio.cds.model.ontology.dao.GenericOntologyDAO;
import se.cambio.cds.model.ontology.dao.GenericOntologyFactory;
import se.cambio.cds.model.ontology.dto.OntologyDTO;
import se.cambio.cds.model.terminology.dao.GenericTerminologyDAO;
import se.cambio.cds.model.terminology.dao.GenericTerminologyFactory;
import se.cambio.cds.model.terminology.dto.TerminologyDTO;
import se.cambio.cds.util.InitialLoadingObservable;
import se.cambio.cds.util.InitialLoadingObservable.LoadingStage;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.handlers.ExceptionHandler;

public class TerminologyServiceImpl implements TerminologyService {

    private TerminologyServiceImpl(InputStream configuration){
	init(configuration);
    }

    public void init(InputStream configuration){
	log.debug("loading config file..");
	try{
	    this.configuration = 
		    TerminologyServiceConfiguration.loadFromProperties(configuration);
	}catch(InternalErrorException e){
	    ExceptionHandler.handle(e);
	}
	log.debug("Initializing the terminology service..");
	this.dataFactories = new HashMap<String, OWLDataFactory>();
	this.reasoners = new HashMap<String, OWLReasoner>();
	this.ontologies = new HashMap<String, OWLOntology>();
	_supportedTerminologies = new HashSet<String>();

	OWLDataFactory dataFactory = null;
	OWLOntology ontology = null;
	OWLReasoner reasoner = null;
	InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.ONTOLOGIES);
	try{
	    //Load ontologies
	    GenericOntologyDAO ontologyDAO = GenericOntologyFactory.getDAO();
	    Collection<OntologyDTO> ontologyDTOs = ontologyDAO.searchAll();

	    int total = ontologyDTOs.size()+1;
	    int count = 1;
	    InitialLoadingObservable.setCurrentProgress((double)count++/total);
	    for (OntologyDTO ontologyDTO : ontologyDTOs) {
		try{
		    log.debug("Loading ontology: " + ontologyDTO.getTerminologyId());
		    InputStream input = new ByteArrayInputStream(ontologyDTO.getSrc());
		    OWLOntologyManager manager = 
			    OWLManager.createOWLOntologyManager();
		    dataFactory = manager.getOWLDataFactory();
		    ontology = manager.loadOntologyFromOntologyDocument(input);
		    OWLReasonerConfiguration config = new SimpleConfiguration();
		    ReasonerFactory risfactory = new ReasonerFactory();
		    reasoner = risfactory.createReasoner(ontology, config);
		    log.debug("Reasoner initialized: " + ontologyDTO.getTerminologyId());
		    this.dataFactories.put(ontologyDTO.getTerminologyId(), dataFactory);
		    this.ontologies.put(ontologyDTO.getTerminologyId(), ontology);
		    this.reasoners.put(ontologyDTO.getTerminologyId(), reasoner);
		    _supportedTerminologies.add(ontologyDTO.getTerminologyId());
		    InitialLoadingObservable.setCurrentProgress((double)count++/total);
		} catch (Exception e) {
		    InternalErrorException iee = new InternalErrorException(new Exception("failed to load ontology '"+ontologyDTO.getTerminologyId()+"'", e));
		    ExceptionHandler.handle(iee);
		    InitialLoadingObservable.addLoadingException(iee);
		}
	    }
	    InitialLoadingObservable.setCurrentLoadingStageFinished();
	}catch(InternalErrorException e){
	    ExceptionHandler.handle(e);
	}
	try{
	    //Load terminologies
	    InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.TERMINOLOGIES);
	    GenericTerminologyDAO terminologyDAO = GenericTerminologyFactory.getDAO();
	    Collection<TerminologyDTO> terminologyDTOs =  terminologyDAO.searchAll();
	    int total = terminologyDTOs.size();
	    int count = 1;
	    for (TerminologyDTO terminologyDTO : terminologyDTOs) {
		try{
		    log.debug("Loading terminology : " + terminologyDTO.getTerminologyId());
		    InputStream input = new ByteArrayInputStream(terminologyDTO.getSrc());
		    String clazz = this.configuration.getPluginSourceClass(terminologyDTO.getTerminologyId());
		    TerminologyServicePlugin terminologyServicePlugin = null;
		    if(clazz != null) {
			try {
			    terminologyServicePlugin = (TerminologyServicePlugin)Class.forName(clazz).newInstance();
			} catch (Exception e) {
			    Logger.getLogger(TerminologyServiceImpl.class).error("ERROR instantiating class '"+clazz+"'");
			}
		    } else {
			terminologyServicePlugin = 
				new CSVTerminologyServicePlugin(terminologyDTO.getTerminologyId());
		    }
		    if (terminologyServicePlugin!=null){
			terminologyServicePlugin.init(input);
			this.registerTerminologyServicePlugin(terminologyServicePlugin);
			_supportedTerminologies.add(terminologyDTO.getTerminologyId());
		    }
		    InitialLoadingObservable.setCurrentProgress((double)count++/total);
		} catch (Exception e) {
		    InternalErrorException iee = new InternalErrorException(new Exception("failed to load terminology '"+terminologyDTO.getTerminologyId()+"'", e));
		    ExceptionHandler.handle(iee);
		    InitialLoadingObservable.addLoadingException(iee);
		}
	    }
	}catch(InternalErrorException e){
	    ExceptionHandler.handle(e);
	}
	InitialLoadingObservable.setCurrentLoadingStageFinished();
    }

    public static TerminologyService getInstance(InputStream configuration){
	if (soleInstance == null) {
	    soleInstance = new TerminologyServiceImpl(configuration);
	}
	return soleInstance;
    }

    public boolean isSubclassOf(CodePhrase a, CodePhrase b)
	    throws UnsupportedTerminologyException, InvalidCodeException {

	log.debug("Checking isSubclassOf (" + a + ", " + b + ")");

	checkTerminologySupported(a);
	checkTerminologySupported(b);

	// TODO Assuming both terminologies are equal
	String terminologyId = a.getTerminologyId().getValue();
	boolean ret = false;
	if (isOntology(terminologyId)) {
	    ret = checkSubclassOf(a, b);
	} else {
	    ret = getTerminologyServicePlugin(terminologyId).isSubclassOf(a, b);
	}

	log.debug("isSubclassOf: " + ret);

	return ret;
    }

    public boolean isSubclassOf(CodePhrase code, Set<CodePhrase> codes)
	    throws UnsupportedTerminologyException, InvalidCodeException {

	log.debug("Checking isSubclassOf (" + code + ", " + codes + ")");

	checkTerminologySupported(code);
	for (CodePhrase cp : codes) {
	    checkTerminologySupported(cp);
	}

	// TODO Assuming both terminologies are equal
	// otherwise mapping files are needed
	String terminologyId = code.getTerminologyId().getValue();
	boolean ret = false;
	if (isOntology(terminologyId)) {
	    log.debug("Checking isSubclassOf using ontology..");
	    for (CodePhrase cp : codes) {
		if (checkSubclassOf(code, cp)) {
		    ret = true;
		    break;
		}
	    }
	} else {
	    log.debug("Checking isSubclassOf using classification..");
	    ret = getTerminologyServicePlugin(terminologyId).isSubclassOf(code,
		    codes);
	}
	log.debug("isSubclassOf: " + ret);

	return ret;
    }

    public boolean isTerminologySupported(String terminologyId) {
	return _supportedTerminologies.contains(terminologyId);
    }

    private boolean isOntology(String terminologyId) {
	return ontologies.containsKey(terminologyId);
    }

    private boolean checkSubclassOf(CodePhrase a, CodePhrase b)
	    throws UnsupportedTerminologyException, InvalidCodeException {

	// TODO: fix support for other terminologies/ontologies using mappings
	String terminology = a.getTerminologyId().getValue();
	OWLDataFactory dataFactory = dataFactories.get(terminology);
	OWLReasoner reasoner = reasoners.get(terminology);

	String url = configuration.terminologyURL(terminology);

	OWLClass subClass = dataFactory.getOWLClass(IRI.create(url
		+ a.getCodeString()));
	OWLClass superClass = dataFactory.getOWLClass(IRI.create(url
		+ b.getCodeString()));
	OWLAxiom axiom = dataFactory
		.getOWLSubClassOfAxiom(subClass, superClass);
	return reasoner.isEntailed(axiom);
    }

    private void checkTerminologySupported(CodePhrase code)
	    throws UnsupportedTerminologyException {
	checkTerminologySupported(code.getTerminologyId().getValue());
    }

    private void checkTerminologySupported(String terminology)
	    throws UnsupportedTerminologyException {
	if (!isTerminologySupported(terminology)) {
	    throw new UnsupportedTerminologyException(terminology
		    + " not supported");
	}
    }

    boolean isTerminologySupported(CodePhrase code) {
	return isTerminologySupported(code.getTerminologyId().getValue());
    }

    public Node retrieveAllSubclasses(CodePhrase concept, CodePhrase language)
	    throws UnsupportedTerminologyException,
	    UnsupportedLanguageException, InvalidCodeException {

	log.debug("retrieve all subclasses of " + concept);

	Node node = null;
	String terminologyId = concept.getTerminologyId().getValue();
	if (isOntology(terminologyId)) {
	    String terminology = concept.getTerminologyId().getValue();
	    OWLDataFactory dataFactory = dataFactories.get(terminology);
	    checkLanguageSupported(language);
	    String url = configuration.terminologyURL(terminology);
	    OWLClass klass = dataFactory.getOWLClass(IRI.create(url
		    + concept.getCodeString()));
	    node = retrieveSubclass(klass, terminology);
	} else {
	    TerminologyService ts = getTerminologyServicePlugin(terminologyId);
	    if (ts != null) {
		node = ts.retrieveAllSubclasses(concept, language);
	    } else {
		throw new UnsupportedTerminologyException(
			"Unknown terminology '" + terminologyId + "'");
	    }
	}
	return node;
    }

    private Node retrieveSubclass(OWLClass klass, String terminology) {
	OWLReasoner reasoner = reasoners.get(terminology);
	String lable = retrieveLable(klass, terminology);
	String url = configuration.terminologyURL(terminology);
	String code = klass.getIRI().toString().substring(url.length());
	Node node = new Node(new DvCodedText(lable, new CodePhrase(terminology,
		code)));
	NodeSet<OWLClass> subclasses = reasoner.getSubClasses(klass, true);

	log.debug("subclasses isEmpty: " + subclasses.isEmpty());

	Set<org.semanticweb.owlapi.reasoner.Node<OWLClass>> nodes = subclasses
		.getNodes();

	for (org.semanticweb.owlapi.reasoner.Node<OWLClass> child : nodes) {
	    OWLClass owlk = child.getRepresentativeElement();
	    if (!owlk.isOWLNothing()) {
		node.addChild(retrieveSubclass(
			child.getRepresentativeElement(), terminology));
	    }
	}
	return node;
    }

    private String retrieveLable(OWLClass klass, String terminology) {
	OWLOntology ontology = ontologies.get(terminology);
	String label = null;
	Set<OWLAnnotation> annotations = klass.getAnnotations(ontology);
	for (OWLAnnotation ann : annotations) {
	    if (ann.getProperty().isLabel()) {
		OWLAnnotationValue v = ann.getValue();
		if (v instanceof OWLLiteral) {
		    OWLLiteral l = (OWLLiteral) v;
		    label = l.getLiteral();
		    break;
		}
	    }
	}
	log.debug("retrieve lable (" + label + ") for class " + klass);

	return label;
    }

    private void checkLanguageSupported(CodePhrase language)
	    throws UnsupportedLanguageException {
	if (!configuration.languageSupported(language)) {
	    throw new UnsupportedLanguageException("Language (" + language
		    + ") not supported.");
	}
    }

    public boolean hasAttributeOfValue(CodePhrase concept,
	    CodePhrase attribute, CodePhrase value)
		    throws UnsupportedTerminologyException, UnknownPropertyException,
		    UnsupportedLanguageException {
	// TODO Auto-generated method stub
	return false;
    }

    public List<DvCodedText> retrieveAllPossibleValues(CodePhrase attribute,
	    CodePhrase language) throws UnsupportedTerminologyException,
	    UnknownPropertyException, UnsupportedLanguageException {
	// TODO Auto-generated method stub
	return null;
    }

    public List<Node> retrieveAll(String terminologyId, CodePhrase language)
	    throws UnsupportedTerminologyException, UnsupportedLanguageException {
	checkLanguageSupported(language);
	if (isOntology(terminologyId)) {
	    // TODO
	    return null;
	} else {
	    TerminologyService ts = getTerminologyServicePlugin(terminologyId);
	    if (ts != null) {
		return ts.retrieveAll(terminologyId, language);
	    } else {
		throw new UnsupportedTerminologyException(
			"Unknown terminology '" + terminologyId + "'");
	    }
	}
    }

    public List<Node> retrieve(String expression, CodePhrase language)
	    throws UnsupportedTerminologyException,
	    UnsupportedLanguageException {
	// TODO Auto-generated method stub
	return null;
    }

    public boolean hasPropertyOfValue(CodePhrase concept, CodePhrase property,
	    CodePhrase value) throws UnsupportedTerminologyException,
	    UnknownPropertyException {
	// TODO Auto-generated method stub
	return false;
    }

    public String retrieveTerm(CodePhrase concept, CodePhrase language)
	    throws UnsupportedTerminologyException,
	    UnsupportedLanguageException {

	String terminologyId = concept.getTerminologyId().getValue();
	if (isOntology(terminologyId)) {
	    // TODO
	    return null;
	} else {
	    TerminologyService ts = getTerminologyServicePlugin(terminologyId);
	    if (ts != null) {
		return ts.retrieveTerm(concept, language);
	    } else {
		throw new UnsupportedTerminologyException(
			"Unknown terminology '" + terminologyId + "'");
	    }
	}
    }

    private TerminologyService getTerminologyServicePlugin(String terminologyId) {
	return getTerminologyServicePluginMap().get(terminologyId);
    }

    public void registerTerminologyServicePlugin(
	    TerminologyServicePlugin terminologyService) {
	getTerminologyServicePluginMap().put(
		terminologyService.getTerminologyId(), terminologyService);
    }

    private Map<String, TerminologyService> getTerminologyServicePluginMap() {
	if (terminologyPlugins == null) {
	    terminologyPlugins = new HashMap<String, TerminologyService>();
	}
	return terminologyPlugins;
    }

    public Collection<String> getSupportedTerminologies() {
	return Collections.unmodifiableCollection(_supportedTerminologies);
    }

    private Map<String, TerminologyService> terminologyPlugins;
    private TerminologyServiceConfiguration configuration;
    private Map<String, OWLOntology> ontologies;
    private Map<String, OWLDataFactory> dataFactories;
    private Map<String, OWLReasoner> reasoners;
    private Set<String> _supportedTerminologies = null;

    private static TerminologyService soleInstance;
    private static Logger log = Logger.getLogger(TerminologyServiceImpl.class);


}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */