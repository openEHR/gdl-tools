package se.cambio.cds.openehr.model.facade.archetype.plain;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import openEHR.v1.template.TEMPLATE;

import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.assertion.Assertion;
import org.openehr.am.archetype.constraintmodel.ArchetypeSlot;
import org.openehr.am.archetype.constraintmodel.CAttribute;
import org.openehr.am.archetype.constraintmodel.CComplexObject;
import org.openehr.am.archetype.constraintmodel.CObject;
import org.openehr.am.archetype.constraintmodel.CPrimitiveObject;
import org.openehr.am.archetype.constraintmodel.primitive.CInteger;
import org.openehr.am.archetype.constraintmodel.primitive.CPrimitive;
import org.openehr.am.archetype.ontology.ArchetypeTerm;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvOrdinal;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvQuantity;
import org.openehr.am.openehrprofile.datatypes.quantity.CDvQuantityItem;
import org.openehr.am.openehrprofile.datatypes.quantity.Ordinal;
import org.openehr.am.openehrprofile.datatypes.text.CCodePhrase;
import org.openehr.am.template.Flattener;
import org.openehr.am.template.OETParser;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;

import se.acode.openehr.parser.ADLParser;
import se.cambio.cds.model.archetype.dao.GenericArchetypeDAO;
import se.cambio.cds.model.archetype.dao.GenericArchetypeFactory;
import se.cambio.cds.model.archetype.dto.ArchetypeDTO;
import se.cambio.cds.model.template.dao.GenericTemplateDAO;
import se.cambio.cds.model.template.dao.GenericTemplateFactory;
import se.cambio.cds.model.template.dto.TemplateDTO;
import se.cambio.cds.openehr.model.archetypeelement.vo.ArchetypeElementVO;
import se.cambio.cds.openehr.model.archetypeslot.vo.ArchetypeSlotVO;
import se.cambio.cds.openehr.model.cluster.vo.ClusterVO;
import se.cambio.cds.openehr.model.codedtext.vo.CodedTextVO;
import se.cambio.cds.openehr.model.facade.archetype.delegate.ArchetypeFacadeDelegate;
import se.cambio.cds.openehr.model.facade.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cds.openehr.model.facade.archetype.vo.TemplateObjectBundleCustomVO;
import se.cambio.cds.openehr.model.ordinal.vo.OrdinalVO;
import se.cambio.cds.openehr.model.proportiontype.vo.ProportionTypeVO;
import se.cambio.cds.openehr.model.unit.vo.UnitVO;
import se.cambio.cds.openehr.util.ExceptionHandler;
import se.cambio.cds.openehr.util.OpenEHRConst;
import se.cambio.cds.openehr.util.OpenEHRDataValuesUI;
import se.cambio.cds.openehr.util.OpenEHRLanguageManager;
import se.cambio.cds.openehr.view.applicationobjects.Archetypes;
import se.cambio.cds.ts.Node;
import se.cambio.cds.util.CDSTerminologyService;
import se.cambio.cds.util.IOUtils;
import se.cambio.cds.util.InitialLoadingObservable;
import se.cambio.cds.util.OpenEHRDataValues;
import se.cambio.cds.util.UserConfigurationManager;
import se.cambio.cds.util.InitialLoadingObservable.LoadingStage;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.MissingConfigurationParameterException;
import se.cambio.cds.util.exceptions.ModelException;

public class PlainArchetypeFacadeDelegate implements ArchetypeFacadeDelegate{

    private static String DEFAULT_LANGUAGE = "en";
    private static String SECTION_NAME = "name/value='";

    public Collection<ArchetypeObjectBundleCustomVO> getAllArchetypesObjectBundles()
	    throws InternalErrorException {
	InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.ARCHETYPES);
	GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
	Collection<ArchetypeDTO> archetypeVOs = dao.getAllArchetypes();
	Collection<ArchetypeObjectBundleCustomVO> archetypeObjectBundleCustomVOs =
		new ArrayList<ArchetypeObjectBundleCustomVO>();
	int total = archetypeVOs.size();
	int count = 1;
	for (ArchetypeDTO archetypeVO : archetypeVOs) {
	    Logger.getLogger(this.getClass()).debug("Parsing archetype '"+archetypeVO.getIdArchetype()+"'...");
	    try{
		ADLParser adlParser = new ADLParser(archetypeVO.getArchetype());
		Archetype ar = adlParser.parse();
		archetypeObjectBundleCustomVOs.add(getArchetypeObjectBundleCustomVO(ar, archetypeVO.getArchetype(), true));
		InitialLoadingObservable.setCurrentProgress((double)count++/total);
	    }catch(Throwable e){
		InternalErrorException iee = new InternalErrorException(new Exception("Failed to parse archetype '"+archetypeVO.getIdArchetype()+"'", e));
		ExceptionHandler.handle(iee);
		InitialLoadingObservable.addLoadingException(iee);
	    }
	}
	InitialLoadingObservable.setCurrentLoadingStageFinished();
	return archetypeObjectBundleCustomVOs;
    }

    public Collection<TemplateObjectBundleCustomVO> getAllTemplateObjectBundles()
	    throws InternalErrorException {
	InitialLoadingObservable.setCurrentLoadingStage(LoadingStage.TEMPLATES);
	GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
	Collection<TemplateDTO> templateDTOs = dao.getAllTemplates();
	Collection<TemplateObjectBundleCustomVO> templateObjectBundleCustomVOs =
		new ArrayList<TemplateObjectBundleCustomVO>();
	int total = templateDTOs.size();
	int count = 1;
	for (TemplateDTO templateDTO : templateDTOs) {
	    Logger.getLogger(this.getClass()).debug("Parsing template '"+templateDTO.getIdTemplate()+"'...");
	    try{
		Archetype ar = null;
		byte[] aomByteArray = templateDTO.getAom();
		if (aomByteArray!=null){
		    ar = (Archetype) IOUtils.getObject(aomByteArray);
		}
		if (ar==null){
		    OETParser parser = new OETParser();
		    InputStream is = IOUtils.toInputStream(templateDTO.getArchetype());
		    TEMPLATE template = parser.parseTemplate(is).getTemplate();
		    Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
		    ar = new Flattener().toFlattenedArchetype(template, archetypeMap);
		}
		templateObjectBundleCustomVOs.add(getTemplateObjectBundleCustomVO(templateDTO.getIdTemplate(), ar, templateDTO.getArchetype(), true));
		InitialLoadingObservable.setCurrentProgress((double)count++/total);
	    }catch(Throwable e){
		InternalErrorException iee = new InternalErrorException(new Exception("Failed to parse template '"+templateDTO.getIdArchetype()+"'", e));
		ExceptionHandler.handle(e);
		InitialLoadingObservable.addLoadingException(iee);
	    }
	}
	InitialLoadingObservable.setCurrentLoadingStageFinished();
	return templateObjectBundleCustomVOs;
    }

    public static ArchetypeObjectBundleCustomVO getArchetypeObjectBundleCustomVO (Archetype ar, String archetypeSrc, boolean loadAll) 
	    throws MissingConfigurationParameterException{
	String language =
		UserConfigurationManager.getLanguage();
	ArchetypeTerm conceptTerm = ar.getOntology().termDefinition(language, ar.getConcept());
	if (conceptTerm==null){
	    //Default
	    language = DEFAULT_LANGUAGE;
	    conceptTerm = ar.getOntology().termDefinition(language, ar.getConcept());
	}
	String name = conceptTerm.getText();
	String description = conceptTerm.getText();
	byte[] aomByteArray = IOUtils.getBytes(ar);
	ArchetypeDTO archetypeVO = 
		new ArchetypeDTO(
			ar.getArchetypeId().getValue(), 
			name, description, 
			ar.getArchetypeId().
			rmEntity(), 
			aomByteArray, 
			archetypeSrc);

	Collection<ArchetypeElementVO> archetypeElementVOs = new ArrayList<ArchetypeElementVO>();
	Collection<ClusterVO> clusterVOs = new ArrayList<ClusterVO>();
	Collection<CodedTextVO> codedTextVOs = null;
	Collection<OrdinalVO> ordinalVOs = null;
	Collection<ArchetypeSlotVO> archetypeSlotVOs = null;
	Collection<UnitVO> unitVOs = null;
	Collection<ProportionTypeVO> proportionTypeVOs = null;

	if(loadAll){
	    codedTextVOs = new ArrayList<CodedTextVO>();
	    ordinalVOs = new ArrayList<OrdinalVO>();
	    archetypeSlotVOs = new ArrayList<ArchetypeSlotVO>();
	    unitVOs = new ArrayList<UnitVO>();
	    proportionTypeVOs = new ArrayList<ProportionTypeVO>();
	}

	loadArchetypeObjects(ar, null, archetypeElementVOs, clusterVOs, codedTextVOs, ordinalVOs, archetypeSlotVOs, unitVOs, proportionTypeVOs, language);

	ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = 
		new ArchetypeObjectBundleCustomVO(
			archetypeVO,
			archetypeElementVOs,
			clusterVOs,
			archetypeSlotVOs,
			codedTextVOs,
			ordinalVOs,
			unitVOs,
			proportionTypeVOs);
	return archetypeObjectBundleCustomVO;
    }

    public static TemplateObjectBundleCustomVO getTemplateObjectBundleCustomVO (String idTemplate, Archetype ar, String archetypeSrc, boolean loadAll) 
	    throws MissingConfigurationParameterException{
	String language =
		UserConfigurationManager.getLanguage();
	ArchetypeTerm conceptTerm = ar.getOntology().termDefinition(language, ar.getConcept());
	if (conceptTerm==null){
	    //Default
	    language = DEFAULT_LANGUAGE;
	    conceptTerm = ar.getOntology().termDefinition(language, ar.getConcept());
	}
	String name = conceptTerm.getText();
	String description = conceptTerm.getText();
	byte[] aomByteArray = IOUtils.getBytes(ar);
	TemplateDTO templateVO = 
		new TemplateDTO(
			idTemplate,
			ar.getArchetypeId().getValue(), 
			name,
			description, 
			ar.getArchetypeId().rmEntity(), 
			aomByteArray, 
			archetypeSrc);

	Collection<ArchetypeElementVO> archetypeElementVOs = new ArrayList<ArchetypeElementVO>();
	Collection<ClusterVO> clusterVOs = new ArrayList<ClusterVO>();
	Collection<CodedTextVO> codedTextVOs = null;
	Collection<OrdinalVO> ordinalVOs = null;
	Collection<ArchetypeSlotVO> archetypeSlotVOs = null;
	Collection<UnitVO> unitVOs = null;
	Collection<ProportionTypeVO> proportionTypeVOs = null;

	if(loadAll){
	    codedTextVOs = new ArrayList<CodedTextVO>();
	    ordinalVOs = new ArrayList<OrdinalVO>();
	    archetypeSlotVOs = new ArrayList<ArchetypeSlotVO>();
	    unitVOs = new ArrayList<UnitVO>();
	    proportionTypeVOs = new ArrayList<ProportionTypeVO>();
	}

	loadArchetypeObjects(
		ar, idTemplate, archetypeElementVOs, clusterVOs, 
		codedTextVOs, ordinalVOs, archetypeSlotVOs, 
		unitVOs, proportionTypeVOs, language);

	TemplateObjectBundleCustomVO templateObjectBundleCustomVO = 
		new TemplateObjectBundleCustomVO(
			templateVO,
			archetypeElementVOs,
			clusterVOs,
			archetypeSlotVOs,
			codedTextVOs,
			ordinalVOs,
			unitVOs,
			proportionTypeVOs);
	return templateObjectBundleCustomVO;
    }

    public static void loadArchetypeObjects(
	    Archetype ar, 
	    String idTemplate,
	    Collection<ArchetypeElementVO> archetypeElementVOs, 
	    Collection<ClusterVO> clusterVOs, 
	    Collection<CodedTextVO> codedTextVOs,
	    Collection<OrdinalVO> ordinalVOs,
	    Collection<ArchetypeSlotVO> archetypeSlotVOs,
	    Collection<UnitVO> unitVOs,
	    Collection<ProportionTypeVO> proportionTypeVOs,
	    String language){
	String archId = ar.getArchetypeId().getValue();
	String rmEntry = ar.getArchetypeId().rmEntity();
	Map<String, CObject> pathObjectMap = ar.getPathNodeMap();
	ArrayList<String> paths = new ArrayList<String>(pathObjectMap.keySet());
	//Shortest path first (to populate clusters before children)
	Collections.sort(paths);
	for (String path : paths) {
	    CObject cObject = pathObjectMap.get(path);
	    if (!OpenEHRConst.PARSABLE_OPENEHR_RM_NAMES.contains(cObject.getRmTypeName())){
		continue;
	    }
	    if (cObject instanceof CComplexObject){
		CAttribute att = ((CComplexObject)cObject).getAttribute("value");
		Archetype localAOM = getLocalAOM(ar, path);
		String text = null;
		String desc = null;
		//Is it referencing an inner archetype?
		ArchetypeDTO archetypeVO = Archetypes.getArchetypeVO(cObject.getNodeId());
		if (archetypeVO==null){
		    text = getText(localAOM, cObject.getNodeId(), language);
		    desc = getDescription(localAOM, cObject.getNodeId(), language);
		}else{
		    text = archetypeVO.getName();
		    desc = archetypeVO.getDescription();
		}
		//TODO ????
		if ("@ internal @".equals(desc) || text==null || text.startsWith("*")){
		    int firstIndex = path.lastIndexOf("/")+1;
		    int finalIndex = path.lastIndexOf("[");
		    if (finalIndex>firstIndex){
			text = path.substring(firstIndex, finalIndex);
		    }
		}
		String type =null;
		CObject childCObject = null;
		if (att!=null){
		    childCObject = att.getChildren().get(0);
		    type = childCObject.getRmTypeName();
		}else{
		    type = cObject.getRmTypeName();
		}
		if (type!=null){
		    if (type.equals("DvOrdinal")){
			type = OpenEHRDataValues.DV_ORDINAL;
		    }else if (type.equals("DvQuantity")){
			type = OpenEHRDataValues.DV_QUANTITY;
		    }
		}
		if (OpenEHRDataValuesUI.isManaged(type)){
		    ArchetypeElementVO archetypeElementVO =
			    new ArchetypeElementVO(text, desc, type, getIdParentCluster(path, clusterVOs), archId, idTemplate, path);
		    archetypeElementVO.setLowerCardinality(cObject.getOccurrences().getLower());
		    archetypeElementVO.setUpperCardinality(cObject.getOccurrences().getUpper());
		    archetypeElementVOs.add(archetypeElementVO);
		    if (OpenEHRDataValues.DV_CODED_TEXT.equals(type)){
			if (codedTextVOs!=null){
			    loadCodedTexts(localAOM, idTemplate, path, childCObject, archetypeElementVO.getId(), language, codedTextVOs);
			}
		    }else if (OpenEHRDataValues.DV_ORDINAL.equals(type)) {
			if (ordinalVOs!=null){
			    loadOrdinals(localAOM, idTemplate, path, childCObject, archetypeElementVO.getId(), language, ordinalVOs);
			}
		    }else if (OpenEHRDataValues.DV_QUANTITY.equals(type)) {
			if (unitVOs!=null){
			    loadUnits(idTemplate, archetypeElementVO.getId(), childCObject, unitVOs);
			}
		    }else if (OpenEHRDataValues.DV_PROPORTION.equals(type)) {
			if (proportionTypeVOs!=null){
			    loadProportionTypes(idTemplate, archetypeElementVO.getId(), childCObject, proportionTypeVOs);
			}
		    }
		}else{
		    if (type.equals(OpenEHRConst.SECTION)){
			String auxText = getSectionName(path);
			//If the user specifies a special name on the template designer
			if (auxText!=null){
			    text = auxText;
			}
		    }
		    ClusterVO clusterVO = new ClusterVO(text, desc, type, getIdParentCluster(path, clusterVOs), archId, idTemplate, path);
		    clusterVO.setLowerCardinality(cObject.getOccurrences().getLower());
		    clusterVO.setUpperCardinality(cObject.getOccurrences().getUpper());
		    clusterVOs.add(clusterVO);
		}
	    }else if (cObject instanceof ArchetypeSlot){
		if (archetypeSlotVOs!=null){
		    ArchetypeSlot archetypeSlot = (ArchetypeSlot) cObject;
		    String text = getText(ar, cObject.getNodeId(), language);
		    if (text==null){
			text=OpenEHRLanguageManager.getMessage("UnnamedSlot");
		    }
		    String desc = getDescription(ar, cObject.getNodeId(), language);
		    if (desc==null){
			desc=OpenEHRLanguageManager.getMessage("UnnamedSlot");
		    }
		    Collection<String> includes = new ArrayList<String>();
		    if (archetypeSlot.getIncludes()!=null){
			for (Assertion assertion : archetypeSlot.getIncludes()) {
			    String exp = assertion.getStringExpression();
			    int indexS= exp.indexOf("{");
			    int indexE= exp.indexOf("}");
			    exp = exp.substring(indexS+2, indexE-1);
			    includes.add(exp);
			}
		    }
		    Collection<String> excludes = new ArrayList<String>();
		    if (archetypeSlot.getExcludes()!=null){
			for (Assertion assertion : archetypeSlot.getExcludes()) {
			    String exp = assertion.getStringExpression();
			    int indexS= exp.indexOf("{");
			    int indexE= exp.indexOf("}");
			    exp = exp.substring(indexS+2, indexE-1);
			    excludes.add(exp);
			}
		    }
		    archetypeSlotVOs.add(
			    new ArchetypeSlotVO(text, desc, cObject.getRmTypeName(), getIdParentCluster(path, clusterVOs), archId, idTemplate, path, includes, excludes));
		}
	    }else if (cObject instanceof ArchetypeSlot){
		Logger.getLogger(PlainArchetypeFacadeDelegate.class).warn("Unkown CObject '"+cObject+"': Skipped");
	    }
	}
	loadRMElements(archId, idTemplate, rmEntry, archetypeElementVOs);
    }

    private static String getIdParentCluster(String path, Collection<ClusterVO> clusterVOs){
	ArrayList<ClusterVO> parentClusters = new ArrayList<ClusterVO>();
	for (ClusterVO clusterVO : clusterVOs) {
	    if (path.startsWith(clusterVO.getPath())){
		parentClusters.add(clusterVO);
	    }
	}
	String idParentCluster = null;
	int length = 0;
	for (ClusterVO clusterVO : parentClusters) {
	    if (clusterVO.getPath().length()>length){
		idParentCluster = clusterVO.getId();
		length = clusterVO.getPath().length();
	    }
	}
	return idParentCluster;
    }

    private static void loadCodedTexts(
	    Archetype ar, 
	    String idTemplate,
	    String path, 
	    CObject childCObject,
	    String idElement,
	    String language,
	    Collection<CodedTextVO> codedTextVOs){
	boolean codedListFound = false;
	if (childCObject instanceof CComplexObject){
	    List<CAttribute> atts = ((CComplexObject)childCObject).getAttributes();
	    if (atts!=null){
		Iterator<CAttribute> atIt = atts.iterator();
		while (atIt.hasNext() && !codedListFound){
		    CAttribute att2 = atIt.next();
		    List<CObject> childAtts = att2.getChildren();
		    Iterator<CObject> childattsIt = childAtts.iterator();
		    while (childattsIt.hasNext() && !codedListFound) {
			CObject cObject = childattsIt.next();
			if (cObject instanceof CCodePhrase){
			    CCodePhrase cCodePhrase = ((CCodePhrase)cObject);
			    //CodePhrase assumed = cCodePhrase.getAssumedValue();
			    if (cCodePhrase.getCodeList()!=null){
				for (String codedStr : cCodePhrase.getCodeList()) {
				    String text = codedStr;
				    String desc = codedStr;
				    CodedTextVO codedText = new CodedTextVO(
					    text,
					    desc,
					    cCodePhrase.getRmTypeName(),
					    idElement,
					    ar.getArchetypeId().getValue(),
					    idTemplate,
					    path, 
					    cCodePhrase.getTerminologyId().getValue(),
					    codedStr,
					    null);
				    if (cCodePhrase.getTerminologyId().getValue().equals(OpenEHRConst.LOCAL)){
					codedText.setName(getText(ar, codedStr, language));
					codedText.setDescription(getDescription(ar, codedStr, language));
				    }else{
					addSubclassCodedTexts(codedText, codedTextVOs);
				    }
				    codedTextVOs.add(codedText);
				};
				codedListFound = true;
			    }
			}
		    }
		}
	    }
	}
    }

    private static void addSubclassCodedTexts(CodedTextVO codedTextVO, Collection<CodedTextVO> codedTextVOs){
	if (!OpenEHRConst.LOCAL.equals(codedTextVO.getTerminology())){
	    try {
		Node node = CDSTerminologyService.retrieveAllSubclasses(
			new CodePhrase(codedTextVO.getTerminology(), codedTextVO.getCode()),
			OpenEHRDataValuesUI.getLanguageCodePhrase());
		DvCodedText ct = node.getValue();
		codedTextVO.setName(getValidCodedTextName(ct.getValue()));
		codedTextVO.setDescription(getValidCodedTextName(ct.getValue()));
		addCodedTextVOs(node, codedTextVO, codedTextVOs);
	    } catch (Exception e){
		ExceptionHandler.handle(e);
	    }
	}
    }

    private static void addCodedTextVOs(Node root, CodedTextVO rootCodedTextVO, Collection<CodedTextVO> codedTextVOs){
	for (Node node : root.getChildren()) {
	    DvCodedText ct = node.getValue();
	    CodedTextVO codedTextVO =  new CodedTextVO(
		    getValidCodedTextName(ct.getValue()), 
		    getValidCodedTextName(ct.getValue()), 
		    OpenEHRDataValues.DV_CODED_TEXT, 
		    rootCodedTextVO.getIdParent(), rootCodedTextVO.getIdArchetype(), rootCodedTextVO.getIdTemplate(), rootCodedTextVO.getPath(), 
		    ct.getDefiningCode().getTerminologyId().getValue(), 
		    ct.getDefiningCode().getCodeString(), rootCodedTextVO);
	    codedTextVOs.add(codedTextVO);
	    addCodedTextVOs(node, codedTextVO, codedTextVOs);
	}
    }

    /* Remove all parenthesis to avoid parsing problems */
    private static String getValidCodedTextName(String string){
	return string.replaceAll("\\(", "[").replaceAll("\\)", "\\]");
    }

    private static void loadOrdinals(
	    Archetype ar,
	    String idTemplate,
	    String path, 
	    CObject childCObject,
	    String idElement,
	    String language, 
	    Collection<OrdinalVO> ordinalVOs){
	if (childCObject instanceof CDvOrdinal){
	    CDvOrdinal cDvOrdinal = (CDvOrdinal)childCObject;
	    if (cDvOrdinal.getList()!=null){
		for (Ordinal ordinal : cDvOrdinal.getList()) {
		    String codedStr = ordinal.getSymbol().getCodeString();
		    String text = codedStr;
		    String desc = codedStr;
		    if (ordinal.getSymbol().getTerminologyId().getValue().equals("local")){
			text = getText(ar, codedStr, language);
			desc = getDescription(ar, codedStr, language);
		    }else{
			Logger.getLogger(PlainArchetypeFacadeDelegate.class).error("Unkown terminology: '"+ordinal.getSymbol().getTerminologyId().getValue()+"', skipping...");
			//TODO TERMINOLOGY SERVICE
		    }
		    ordinalVOs.add(
			    new OrdinalVO(
				    text,
				    desc,
				    cDvOrdinal.getRmTypeName(),
				    idElement,
				    ar.getArchetypeId().getValue(),
				    idTemplate,
				    path, 
				    ordinal.getValue(),
				    ordinal.getSymbol().getTerminologyId().getValue(),
				    ordinal.getSymbol().getCodeString()));
		};
	    }
	}
    }



    private static void loadRMElements(String idArchetype, String idTemplate, String entryType, Collection<ArchetypeElementVO> archetypeElementVOs){
	if (OpenEHRConst.OBSERVATION.equals(entryType)){
	    /*Origin (Use EventTime instead)
	    archetypeElementVOs.add(
		    new ArchetypeElementVO(
			    LanguageManager.getMessage("Origin"), 
			    LanguageManager.getMessage("OriginDesc"),
			    OpenEHRDataValues.DV_DATE_TIME, null, 
			    idArchetype, "/time"));
	     */
	    //EventTime
	    archetypeElementVOs.add(
		    new ArchetypeElementVO(
			    OpenEHRLanguageManager.getMessage("EventTime"), 
			    OpenEHRLanguageManager.getMessage("EventTimeDesc"),
			    OpenEHRDataValues.DV_DATE_TIME, null, 
			    idArchetype, idTemplate, "/event/time"));
	}else if (OpenEHRConst.INSTRUCTION.equals(entryType)){
	    //Expiry Time
	    archetypeElementVOs.add(
		    new ArchetypeElementVO(
			    OpenEHRLanguageManager.getMessage("ExpireTime"), 
			    OpenEHRLanguageManager.getMessage("ExpireTimeDesc"),
			    OpenEHRDataValues.DV_DATE_TIME, null, 
			    idArchetype, idTemplate, "/expiry_time"));
	    //Expiry Time
	    archetypeElementVOs.add(
		    new ArchetypeElementVO(
			    OpenEHRLanguageManager.getMessage("NarrativeDescription"), 
			    OpenEHRLanguageManager.getMessage("NarrativeDescriptionDesc"),
			    OpenEHRDataValues.DV_TEXT, null, 
			    idArchetype, idTemplate, "/narrative"));
	}else if (OpenEHRConst.ACTION.equals(entryType)){
	    //Date and time Action step performed
	    archetypeElementVOs.add(
		    new ArchetypeElementVO(
			    OpenEHRLanguageManager.getMessage("DateTimeActionPerformed"), 
			    OpenEHRLanguageManager.getMessage("DateTimeActionPerformedDesc"),
			    OpenEHRDataValues.DV_DATE_TIME, null, 
			    idArchetype, idTemplate, "/time"));
	    //Current Action State
	    archetypeElementVOs.add(
		    new ArchetypeElementVO(
			    OpenEHRLanguageManager.getMessage("CurrentActionState"), 
			    OpenEHRLanguageManager.getMessage("CurrentActionStateDesc"),
			    OpenEHRDataValues.DV_DATE_TIME, null, 
			    idArchetype, idTemplate, "/ism_transition/current_state"));
	}else if (OpenEHRConst.EVALUATION.equals(entryType)){

	}
	//Template Id
	archetypeElementVOs.add(
		new ArchetypeElementVO(
			OpenEHRLanguageManager.getMessage("TemplateId"), 
			OpenEHRLanguageManager.getMessage("TemplateIdDesc"),
			OpenEHRDataValues.DV_TEXT, null, 
			idArchetype, idTemplate, "/archetype_details/template_id"));
    }

    private static void loadUnits(
	    String idTemplate,
	    String idElement,
	    CObject childCObject,
	    Collection<UnitVO> unitVOs){
	if (childCObject instanceof CDvQuantity){
	    CDvQuantity cDvQuantity = (CDvQuantity)childCObject;
	    if (cDvQuantity.getList()!=null){
		for (CDvQuantityItem cDvQuantityItem : cDvQuantity.getList()) {
		    unitVOs.add(new UnitVO(idTemplate, idElement, cDvQuantityItem.getUnits()));
		};
	    }
	}
    }

    private static void loadProportionTypes(
	    String idTemplate,
	    String idElement,
	    CObject childCObject,
	    Collection<ProportionTypeVO> proportionTypeVOs){
	if (childCObject instanceof CComplexObject){
	    CComplexObject cComplexObject = (CComplexObject)childCObject;
	    CAttribute cAttribute = cComplexObject.getAttribute("type");
	    if (cAttribute!=null){
		for (CObject cObject : cAttribute.getChildren()) {
		    if (cObject instanceof CPrimitiveObject){
			CPrimitive cPrimitive = ((CPrimitiveObject) cObject).getItem();
			if (cPrimitive instanceof CInteger){
			    CInteger cInteger = (CInteger) cPrimitive;
			    for (Integer proportionType : cInteger.getList()) {
				proportionTypeVOs.add(new ProportionTypeVO(idTemplate, idElement, proportionType));	
			    }
			}
		    }
		};
	    }
	}
    }

    private static String getText(Archetype ar, String codeStr, String language){
	ArchetypeTerm term = getArchetypeTerm(ar, codeStr, language);
	if (term!=null){
	    return term.getText();
	}else{
	    return null;
	}
    }

    private static String getDescription(Archetype ar, String codeStr, String language){
	ArchetypeTerm term = getArchetypeTerm(ar, codeStr, language);
	if (term!=null){
	    return term.getDescription();
	}else{
	    return null;
	}
    }

    private static ArchetypeTerm getArchetypeTerm(Archetype ar, String codeStr, String language){
	ArchetypeTerm term = ar.getOntology().termDefinition(language, codeStr);
	if (term==null){
	    term = ar.getOntology().termDefinition(ar.getOriginalLanguage().getCodeString(), codeStr);
	}
	return term;
    }

    public ArchetypeObjectBundleCustomVO addArchetype(ArchetypeDTO archetypeVO)
	    throws InternalErrorException, ModelException{
	try{
	    GenericArchetypeDAO dao = GenericArchetypeFactory.getDAO();
	    dao.addArchetype(archetypeVO);
	    ADLParser adlParser = new ADLParser(archetypeVO.getArchetype());
	    Archetype ar = adlParser.parse();
	    return getArchetypeObjectBundleCustomVO(ar, archetypeVO.getArchetype(), true);
	}catch(MissingConfigurationParameterException e){
	    throw new InternalErrorException(e);
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    public TemplateObjectBundleCustomVO addTemplate(TemplateDTO templateVO) 
	    throws InternalErrorException, ModelException{
	try{
	    GenericTemplateDAO dao = GenericTemplateFactory.getDAO();
	    dao.addTemplate(templateVO);
	    Archetype ar = (Archetype)IOUtils.getObject(templateVO.getAom());
	    return getTemplateObjectBundleCustomVO(templateVO.getIdTemplate(), ar, templateVO.getArchetype(), true);
	}catch(MissingConfigurationParameterException e){
	    throw new InternalErrorException(e);
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }
    public String getIdArchetype(String archetypeSrc)
	    throws InternalErrorException, ModelException{
	try{
	    ADLParser adlParser = new ADLParser(archetypeSrc);
	    return adlParser.parse().getArchetypeId().getValue();
	}catch(Exception e){
	    throw new InternalErrorException(e);
	}
    }

    public static Archetype getLocalAOM(Archetype currentAOM, String path){
	int i = path.lastIndexOf("[");
	while(i>0){
	    String idArchetype = path.substring(i+1, path.lastIndexOf("]"));
	    if (Archetypes.getArchetypeVO(idArchetype)!=null){
		return Archetypes.getArchetypeAOM(idArchetype);
	    }else{
		path = path.substring(0,i);
		i = path.lastIndexOf("[");
	    }
	};
	return currentAOM;
    }

    private static String getSectionName(String path){
	int i1 = path.lastIndexOf(SECTION_NAME)+SECTION_NAME.length();
	int i2 = path.indexOf("'",i1);
	if (i1>0 && i2>0){
	    return path.substring(i1, i2);
	}else{
	    return null;
	}
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