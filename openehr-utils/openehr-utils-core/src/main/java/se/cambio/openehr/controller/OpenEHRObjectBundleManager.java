package se.cambio.openehr.controller;

import openEHR.v1.template.TEMPLATE;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.assertion.Assertion;
import org.openehr.am.archetype.constraintmodel.*;
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
import se.acode.openehr.parser.ParseException;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.*;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.ModelException;

import java.io.InputStream;
import java.util.*;

public class OpenEHRObjectBundleManager {
    private static String SECTION_NAME = "name/value='";

    public static void generateArchetypesObjectBundles(Collection<ArchetypeDTO> archetypeDTOs)
            throws InternalErrorException {
        int total = archetypeDTOs.size();
        int count = 1;
        for (ArchetypeDTO archetypeDTO : archetypeDTOs) {
            if (archetypeDTO.getAobcVO()!=null){
                ArchetypeObjectBundleCustomVO aobcVO = (ArchetypeObjectBundleCustomVO)IOUtils.getObject(archetypeDTO.getAobcVO());
                if (aobcVO==null){
                    //Force new parsing
                    archetypeDTO.setAobcVO(null);
                }
            }
            if (archetypeDTO.getAobcVO()==null){
                Logger.getLogger(OpenEHRObjectBundleManager.class).debug("Parsing archetype '"+archetypeDTO.getIdArchetype()+"'...");
                try{
                    generateArchetypeObjectBundleCustomVO(archetypeDTO);
                }catch(Throwable e){
                    InternalErrorException iee = new InternalErrorException(new Exception("Failed to parse archetype '"+archetypeDTO.getIdArchetype()+"'", e));
                    ExceptionHandler.handle(iee);
                }
            }
        }
    }

    public static void generateTemplateObjectBundles(Collection<TemplateDTO> templateDTOs)
            throws InternalErrorException {
        int total = templateDTOs.size();
        int count = 1;
        Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
        for (TemplateDTO templateDTO : templateDTOs) {
            Logger.getLogger(OpenEHRObjectBundleManager.class).debug("Parsing template '"+templateDTO.getIdTemplate()+"'...");
            if (templateDTO.getTobcVO()!=null){
                TemplateObjectBundleCustomVO tobcVO = (TemplateObjectBundleCustomVO)IOUtils.getObject(templateDTO.getTobcVO());
                if (tobcVO==null){
                    //Force new parsing
                    templateDTO.setTobcVO(null);
                }
            }
            if (templateDTO.getTobcVO()==null){
                try{
                    generateTemplateObjectBundleCustomVO(templateDTO, archetypeMap);
                }catch(Throwable e){
                    InternalErrorException iee = new InternalErrorException(new Exception("Failed to parse template '"+templateDTO.getIdTemplate()+"'", e));
                    ExceptionHandler.handle(iee);
                }
            }
        }
    }

    public static void generateArchetypeObjectBundleCustomVO (ArchetypeDTO archetypeDTO)
            throws ParseException, Exception{
        ADLParser adlParser = new ADLParser(archetypeDTO.getArchetype());
        Archetype ar = adlParser.parse();
        String language =
                UserConfigurationManager.getLanguage();
        if (!ar.getOriginalLanguage().getCodeString().equals(language)
                && (ar.getTranslations()==null || !ar.getTranslations().containsKey(language))){
            //Default
            language = ar.getOriginalLanguage().getCodeString();
        }
        Collection<ArchetypeElementVO> archetypeElementVOs = new ArrayList<ArchetypeElementVO>();
        Collection<ClusterVO> clusterVOs = new ArrayList<ClusterVO>();
        Collection<CodedTextVO> codedTextVOs = null;
        Collection<OrdinalVO> ordinalVOs = null;
        Collection<ArchetypeSlotVO> archetypeSlotVOs = null;
        Collection<UnitVO> unitVOs = null;
        Collection<ProportionTypeVO> proportionTypeVOs = null;
        codedTextVOs = new ArrayList<CodedTextVO>();
        ordinalVOs = new ArrayList<OrdinalVO>();
        archetypeSlotVOs = new ArrayList<ArchetypeSlotVO>();
        unitVOs = new ArrayList<UnitVO>();
        proportionTypeVOs = new ArrayList<ProportionTypeVO>();
        loadArchetypeObjects(ar, null, archetypeElementVOs, clusterVOs, codedTextVOs, ordinalVOs, archetypeSlotVOs, unitVOs, proportionTypeVOs, language);

        ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO =
                new ArchetypeObjectBundleCustomVO(
                        archetypeElementVOs,
                        clusterVOs,
                        archetypeSlotVOs,
                        codedTextVOs,
                        ordinalVOs,
                        unitVOs,
                        proportionTypeVOs);
        archetypeDTO.setRMName(ar.getArchetypeId().rmEntity());
        archetypeDTO.setAom(IOUtils.getBytes(ar));
        archetypeDTO.setAobcVO(IOUtils.getBytes(archetypeObjectBundleCustomVO));
    }

    public static void generateTemplateObjectBundleCustomVO (TemplateDTO templateDTO, Map<String, Archetype> archetypeMap)
            throws Exception{
        String language =
                UserConfigurationManager.getLanguage();
        Archetype ar = null;
        byte[] aomByteArray = templateDTO.getAom();
        if (aomByteArray!=null){
            ar = (Archetype) IOUtils.getObject(aomByteArray);
        }
        if (ar==null){
            OETParser parser = new OETParser();
            InputStream is = IOUtils.toInputStream(templateDTO.getArchetype());
            TEMPLATE template = parser.parseTemplate(is).getTemplate();
            templateDTO.setName(template.getName());
            //TODO
            if (template.getDescription()!=null && template.getDescription().getDetails()!=null&&template.getDescription().getDetails().getPurpose()!=null){
                templateDTO.setDescription(template.getDescription().getDetails().getPurpose());
            }
            ar = new Flattener().toFlattenedArchetype(template, archetypeMap);
        }
        if (!ar.getOriginalLanguage().getCodeString().equals(language) &&
                (ar.getTranslations()==null || !ar.getTranslations().containsKey(language))){
            //Default
            language = ar.getOriginalLanguage().getCodeString();
        }

        Collection<ArchetypeElementVO> archetypeElementVOs = new ArrayList<ArchetypeElementVO>();
        Collection<ClusterVO> clusterVOs = new ArrayList<ClusterVO>();
        Collection<CodedTextVO> codedTextVOs = null;
        Collection<OrdinalVO> ordinalVOs = null;
        Collection<ArchetypeSlotVO> archetypeSlotVOs = null;
        Collection<UnitVO> unitVOs = null;
        Collection<ProportionTypeVO> proportionTypeVOs = null;

        codedTextVOs = new ArrayList<CodedTextVO>();
        ordinalVOs = new ArrayList<OrdinalVO>();
        archetypeSlotVOs = new ArrayList<ArchetypeSlotVO>();
        unitVOs = new ArrayList<UnitVO>();
        proportionTypeVOs = new ArrayList<ProportionTypeVO>();

        loadArchetypeObjects(
                ar, templateDTO.getIdTemplate(), archetypeElementVOs, clusterVOs,
                codedTextVOs, ordinalVOs, archetypeSlotVOs,
                unitVOs, proportionTypeVOs, language);

        TemplateObjectBundleCustomVO templateObjectBundleCustomVO =
                new TemplateObjectBundleCustomVO(
                        archetypeElementVOs,
                        clusterVOs,
                        archetypeSlotVOs,
                        codedTextVOs,
                        ordinalVOs,
                        unitVOs,
                        proportionTypeVOs);

        templateDTO.setIdArchetype(ar.getArchetypeId().getValue());
        templateDTO.setRMName(ar.getArchetypeId().rmEntity());
        templateDTO.setAom(IOUtils.getBytes(ar));
        templateDTO.setTobcVO(IOUtils.getBytes(templateObjectBundleCustomVO));
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
        //Collections.sort(paths);
        //for (String path : paths) {
            //CObject cObject = pathObjectMap.get(path);
            proccessCObject(ar.getDefinition(), ar, archId, idTemplate, archetypeElementVOs, clusterVOs, codedTextVOs, ordinalVOs, archetypeSlotVOs, unitVOs, proportionTypeVOs, language);
        //}
        loadRMElements(archId, idTemplate, rmEntry, archetypeElementVOs);
    }

    private static void proccessCObject(
            CObject cObject,
            Archetype ar,
            String archId,
            String idTemplate,
            Collection<ArchetypeElementVO> archetypeElementVOs,
            Collection<ClusterVO> clusterVOs,
            Collection<CodedTextVO> codedTextVOs,
            Collection<OrdinalVO> ordinalVOs,
            Collection<ArchetypeSlotVO> archetypeSlotVOs,
            Collection<UnitVO> unitVOs,
            Collection<ProportionTypeVO> proportionTypeVOs,
            String language
    ){
        String path = cObject.path();
        if (!OpenEHRConst.PARSABLE_OPENEHR_RM_NAMES.contains(cObject.getRmTypeName())){
            return;
        }
        if (cObject instanceof CComplexObject){
            CComplexObject cComplexObject = ((CComplexObject)cObject);
            CAttribute att = cComplexObject.getAttribute("value");
            Archetype localAOM = getLocalAOM(ar, path);
            String text = null;
            String desc = null;
            //Is it referencing an inner archetype?
            ArchetypeDTO archetypeVO = Archetypes.getArchetypeDTO(cObject.getNodeId());
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
            String type = null;
            CObject childCObject = null;
            if (att!=null){
                if (att.getChildren()!=null && !att.getChildren().isEmpty()){
                    childCObject = att.getChildren().get(0);
                    type = childCObject.getRmTypeName();
                }else{
                    type = cObject.getRmTypeName();
                }
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
                setCardinalities(archetypeElementVO, cObject);
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
                setCardinalities(clusterVO, cObject);
                clusterVOs.add(clusterVO);
            }

            //Recursive lookup
            for (CAttribute cAttribute: cComplexObject.getAttributes()){
                for (CObject cObjectAux : cAttribute.getChildren()){
                    proccessCObject(cObjectAux, ar, archId, idTemplate, archetypeElementVOs, clusterVOs, codedTextVOs, ordinalVOs, archetypeSlotVOs, unitVOs, proportionTypeVOs, language);
                }
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
        }else {
            //Archetype internal refs, etc.
            //Logger.getLogger(OpenEHRObjectBundleManager.class).debug("Unkown CObject '"+cObject+"': Skipped");
        }
    }

    private static void setCardinalities(PathableVO pathableVO, CObject cObject){
        //TODO
        pathableVO.setLowerCardinality(cObject.getOccurrences().getLower());
        pathableVO.setUpperCardinality(cObject.getOccurrences().getUpper());
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
                TerminologyNodeVO node =
                        OpenEHRSessionManager.getTerminologyFacadeDelegate().retrieveAllSubclasses(
                                new CodePhrase(codedTextVO.getTerminology(), codedTextVO.getCode()),
                                OpenEHRDataValuesUI.getLanguageCodePhrase());
                if (node==null){
                    Logger.getLogger(OpenEHRObjectBundleManager.class).warn("Terminology code not found '"+codedTextVO.getCode()+"::"+codedTextVO.getTerminology()+"'.");
                    return;
                }
                DvCodedText ct = node.getValue();
                codedTextVO.setName(getValidCodedTextName(ct.getValue()));
                codedTextVO.setDescription(getValidCodedTextName(ct.getValue()));
                addCodedTextVOs(node, codedTextVO, codedTextVOs);
            } catch (Exception e){
                ExceptionHandler.handle(e);
            }
        }
    }

    private static void addCodedTextVOs(TerminologyNodeVO root, CodedTextVO rootCodedTextVO, Collection<CodedTextVO> codedTextVOs){
        for (TerminologyNodeVO node : root.getChildren()) {
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
                        Logger.getLogger(OpenEHRObjectBundleManager.class).error("Unkown terminology: '"+ordinal.getSymbol().getTerminologyId().getValue()+"', skipping...");
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
            //Narrative Description
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

    public static void addArchetype(ArchetypeDTO archetypeDTO)
            throws InternalErrorException, ModelException{
        try{
            OpenEHRSessionManager.getAdministrationFacadeDelegate().upsertArchetype(archetypeDTO);
        }catch(Exception e){
            throw new InternalErrorException(e);
        }
    }

    public static void addTemplate(TemplateDTO templateDTO)
            throws InternalErrorException, ModelException{
        try{
            OpenEHRSessionManager.getAdministrationFacadeDelegate().upsertTemplate(templateDTO);
        }catch(Exception e){
            throw new InternalErrorException(e);
        }
    }

    public static Archetype getLocalAOM(Archetype currentAOM, String path){
        int i = path.lastIndexOf("[");
        while(i>0){
            String idArchetype = path.substring(i+1, path.lastIndexOf("]"));
            if (Archetypes.getArchetypeDTO(idArchetype)!=null){
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
