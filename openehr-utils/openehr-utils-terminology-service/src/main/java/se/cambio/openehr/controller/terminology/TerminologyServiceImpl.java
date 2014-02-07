package se.cambio.openehr.controller.terminology;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.controller.terminology.plugins.CSVTerminologyServicePlugin;
import se.cambio.openehr.controller.terminology.plugins.TerminologyServicePlugin;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.*;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.*;

public class TerminologyServiceImpl implements TerminologyService {

    private Map<String, TerminologyService> terminologyPlugins;
    /*TODO Ontology support:
    private Map<String, OWLOntology> ontologies;
    private Map<String, OWLDataFactory> dataFactories;
    private Map<String, OWLReasoner> reasoners;
     */
    private Set<String> _supportedTerminologies = null;
    private static TerminologyServiceImpl soleInstance;
    private static Logger log = Logger.getLogger(TerminologyServiceImpl.class);

    public TerminologyServiceImpl(){
        init();
    }

    public void init(){
        _supportedTerminologies = new HashSet<String>();
        //TODO Remove initialLoadingObservable
        //InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.ONTOLOGIES);
	/*TODO  Ontology support: Load ontologies
	this.dataFactories = new HashMap<String, OWLDataFactory>();
	this.reasoners = new HashMap<String, OWLReasoner>();
	this.ontologies = new HashMap<String, OWLOntology>();

	OWLDataFactory dataFactory = null;
	OWLOntology ontology = null;
	OWLReasoner reasoner = null;

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

	}catch(InternalErrorException e){
	    ExceptionHandler.handle(e);
	}
	 */
        if (!Terminologies.isLoaded()){
            try {
                Terminologies.loadTerminologies();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }

        for (TerminologyDTO terminologyDTO : Terminologies.getAllTerminologies()) {
            try{
                log.debug("Loading terminology : " + terminologyDTO.getTerminologyId());
                InputStream input = new ByteArrayInputStream(terminologyDTO.getSrc());
                String clazz = TerminologyServiceConfiguration.getPluginSourceClass(terminologyDTO.getTerminologyId());
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
            } catch (Exception e) {
                InternalErrorException iee = new InternalErrorException(new Exception("failed to load terminology '"+terminologyDTO.getTerminologyId()+"'", e));
                ExceptionHandler.handle(iee);
            }
        }
    }

    public static TerminologyServiceImpl getInstance(){
        if (soleInstance == null) {
            soleInstance = new TerminologyServiceImpl();
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
	    /*TODO Ontology support:
	    ret = checkSubclassOf(a, b);
	     */
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
	    /*TODO Ontology support:
	    log.debug("Checking isSubclassOf using ontology..");
	    for (CodePhrase cp : codes) {
		if (checkSubclassOf(code, cp)) {
		    ret = true;
		    break;
		}
	    }
	     */
        } else {
            log.debug("Checking isSubclassOf using classification..");
            ret = getTerminologyServicePlugin(terminologyId).isSubclassOf(code, codes);
        }
        log.debug("isSubclassOf: " + ret);
        return ret;
    }

    public boolean isTerminologySupported(String terminologyId) {
        return _supportedTerminologies.contains(terminologyId);
    }

    private boolean isOntology(String terminologyId) {
        return false;//TODO Ontology support: ontologies.containsKey(terminologyId);
    }

    /*TODO Ontology support:
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
     */

    private void checkTerminologySupported(CodePhrase code)
            throws UnsupportedTerminologyException {
        checkTerminologySupported(code.getTerminologyId().getValue());
    }

    private void checkTerminologySupported(String terminology)
            throws UnsupportedTerminologyException {
        if (!isTerminologySupported(terminology)) {
            throw new UnsupportedTerminologyException(terminology + " not supported");
        }
    }

    public boolean isTerminologySupported(CodePhrase code) {
        return isTerminologySupported(code.getTerminologyId().getValue());
    }

    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language)
            throws UnsupportedTerminologyException,
            UnsupportedLanguageException, InvalidCodeException {
        log.debug("retrieve all subclasses of " + concept);
        TerminologyNodeVO node = null;
        String terminologyId = concept.getTerminologyId().getValue();
        if (isOntology(terminologyId)) {
	    /*TODO Ontology support:
	    String terminology = concept.getTerminologyId().getValue();
	    OWLDataFactory dataFactory = dataFactories.get(terminology);
	    checkLanguageSupported(language);
	    String url = configuration.terminologyURL(terminology);
	    OWLClass klass = dataFactory.getOWLClass(IRI.create(url
		    + concept.getCodeString()));
	    node = retrieveSubclass(klass, terminology);
	     */
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
    /*TODO Ontology support:
    private TerminologyNodeVO retrieveSubclass(OWLClass klass, String terminology) {
	OWLReasoner reasoner = reasoners.get(terminology);
	String lable = retrieveLable(klass, terminology);
	String url = configuration.terminologyURL(terminology);
	String code = klass.getIRI().toString().substring(url.length());
	TerminologyNodeVO node = new TerminologyNodeVO(new DvCodedText(lable, new CodePhrase(terminology,
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
     */
    private void checkLanguageSupported(CodePhrase language)
            throws UnsupportedLanguageException {
        if (!TerminologyServiceConfiguration.languageSupported(language)) {
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

    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language)
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

    public List<TerminologyNodeVO> retrieve(String expression, CodePhrase language)
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

    public boolean isValidCodePhrase(CodePhrase codePhrase) {
        String terminologyId = codePhrase.getTerminologyId().getValue();
        if (isOntology(terminologyId)) {
            // TODO Check valid codes
            return true;
        } else {
            TerminologyService ts = getTerminologyServicePlugin(terminologyId);
            if (ts != null) {
                return ts.isValidCodePhrase(codePhrase);
            } else {
                return false;
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