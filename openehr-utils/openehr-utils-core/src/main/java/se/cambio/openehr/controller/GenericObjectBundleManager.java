package se.cambio.openehr.controller;

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
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.*;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class GenericObjectBundleManager {
    private static String SECTION_NAME = "name/value='";
    protected String templateId = null;
    private String language = null;
    private Archetype ar = null;
    private Collection<ArchetypeElementVO> archetypeElementVOs = new ArrayList<ArchetypeElementVO>();
    private Collection<ClusterVO> clusterVOs = new ArrayList<ClusterVO>();
    private Collection<CodedTextVO> codedTextVOs = null;
    private Collection<OrdinalVO> ordinalVOs = null;
    private Collection<ArchetypeSlotVO> archetypeSlotVOs = null;
    private Collection<UnitVO> unitVOs = null;
    private Collection<ProportionTypeVO> proportionTypeVOs = null;

    public GenericObjectBundleManager(Archetype ar) {
        this.ar = ar;
    }

    public GenericObjectBundleManager(Archetype ar, String templateId) {
        this.ar = ar;
        this.templateId = templateId;
    }

    public ArchetypeObjectBundleCustomVO generateObjectBundleCustomVO(){
        init();
        setDefaultLanguage();
        loadArchetypeObjects();
        return new ArchetypeObjectBundleCustomVO(
                archetypeElementVOs,
                clusterVOs,
                archetypeSlotVOs,
                codedTextVOs,
                ordinalVOs,
                unitVOs,
                proportionTypeVOs);
    }

    private void setDefaultLanguage() {
        language = UserConfigurationManager.getLanguage();
        if (!ar.getOriginalLanguage().getCodeString().equals(language) &&
                (ar.getTranslations() == null || !ar.getTranslations().containsKey(language))){
            language = ar.getOriginalLanguage().getCodeString();
        }
    }

    private void init() {
        codedTextVOs = new ArrayList<CodedTextVO>();
        ordinalVOs = new ArrayList<OrdinalVO>();
        archetypeSlotVOs = new ArrayList<ArchetypeSlotVO>();
        unitVOs = new ArrayList<UnitVO>();
        proportionTypeVOs = new ArrayList<ProportionTypeVO>();
    }

    public void loadArchetypeObjects(){
        String archId = ar.getArchetypeId().getValue();
        String rmEntry = ar.getArchetypeId().rmEntity();
        proccessCObject(ar.getDefinition());
        Collection<ArchetypeElementVO> rmArchetypeElements = OpenEHRRMUtil.getRMElements(archId, templateId, rmEntry);
        archetypeElementVOs.addAll(rmArchetypeElements);
    }

    private void proccessCObject(CObject cObject){
        String path = cObject.path();
        String archetypeId = ar.getArchetypeId().getValue();
        if (!OpenEHRConst.PARSABLE_OPENEHR_RM_NAMES.contains(cObject.getRmTypeName())){
            return;
        }
        if (cObject instanceof CComplexObject){
            processComplexObject(cObject, path, archetypeId);
        }else if (cObject instanceof ArchetypeSlot){
            proccessArchetypeSlot(cObject, path, archetypeId);
        }
    }

    private void proccessArchetypeSlot(CObject cObject, String path, String archetypeId) {
        ArchetypeSlot archetypeSlot = (ArchetypeSlot) cObject;
        String text = getText(ar, cObject.getNodeId(), language);
        if (text == null){
            text= OpenEHRLanguageManager.getMessage("UnnamedSlot");
        }
        String desc = getDescription(ar, cObject.getNodeId(), language);
        if (desc == null){
            desc=OpenEHRLanguageManager.getMessage("UnnamedSlot");
        }
        Collection<String> includes = new ArrayList<String>();
        if (archetypeSlot.getIncludes() != null){
            for (Assertion assertion : archetypeSlot.getIncludes()) {
                String exp = assertion.getStringExpression();
                int indexS= exp.indexOf("{");
                int indexE= exp.indexOf("}");
                exp = exp.substring(indexS+2, indexE-1);
                includes.add(exp);
            }
        }
        Collection<String> excludes = new ArrayList<String>();
        if (archetypeSlot.getExcludes() != null){
            for (Assertion assertion : archetypeSlot.getExcludes()) {
                String exp = assertion.getStringExpression();
                int indexS= exp.indexOf("{");
                int indexE= exp.indexOf("}");
                exp = exp.substring(indexS+2, indexE-1);
                excludes.add(exp);
            }
        }
        ArchetypeSlotVO archetypeSlotVO =
                new ArchetypeSlotVO(text, desc, cObject.getRmTypeName(), getIdParentCluster(path, clusterVOs), archetypeId, templateId, path, includes, excludes);
        archetypeSlotVOs.add(archetypeSlotVO);
    }

    private void processComplexObject(CObject cObject, String path, String archetypeId) {
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
            if (finalIndex > firstIndex){
                text = path.substring(firstIndex, finalIndex);
            }
        }
        CObject childCObject = getCChildCObject(att);
        String type = getType(cObject, att);
        if (hasCardinalityZero(cComplexObject)){
            return;
        }
        if (OpenEHRDataValuesUI.isManaged(type)){
            processDataValue(cObject, path, archetypeId, localAOM, text, desc, type, childCObject);
        }else{
            processSectionsAndClusters(cObject, path, archetypeId, text, desc, type);
        }

        //Recursive lookup
        for (CAttribute cAttribute: cComplexObject.getAttributes()){
            for (CObject cObjectAux : cAttribute.getChildren()){
                proccessCObject(cObjectAux);
            }
        }
    }

    private String getType(CObject cObject, CAttribute att) {
        String type;
        if (att != null){
            if (att.getChildren() != null && !att.getChildren().isEmpty()){
                type = att.getChildren().get(0).getRmTypeName();
            }else{
                type = cObject.getRmTypeName();
            }
        }else{
            type = cObject.getRmTypeName();
        }
        type = convertTypeIfQuantityOrOrdinal(type);
        return type;
    }

    private CObject getCChildCObject(CAttribute att) {
        CObject childCObject = null;
        if ((att != null) && (att.getChildren() != null) && !att.getChildren().isEmpty()) {
            childCObject = att.getChildren().get(0);
        }
        return childCObject;
    }

    private boolean hasCardinalityZero(CComplexObject cComplexObject) {
        return (cComplexObject.getOccurrences() != null) &&
                (cComplexObject.getOccurrences().getUpper() != null) &&
                (cComplexObject.getOccurrences().getUpper() <= 0);
    }

    private String convertTypeIfQuantityOrOrdinal(String type) {
        if (type != null){
            if (type.equals("DvOrdinal")){
                type = OpenEHRDataValues.DV_ORDINAL;
            }else if (type.equals("DvQuantity")){
                type = OpenEHRDataValues.DV_QUANTITY;
            }
        }
        return type;
    }

    private void processSectionsAndClusters(CObject cObject, String path, String archetypeId, String text, String desc, String type) {
        if (type.equals(OpenEHRConst.SECTION)){
            String auxText = getSectionName(path);
            //If the user specifies a special name on the template designer
            if (auxText != null){
                text = auxText;
            }
        }
        ClusterVO clusterVO = new ClusterVO(text, desc, type, getIdParentCluster(path, clusterVOs), archetypeId, templateId, path);
        setCardinalities(clusterVO, cObject);
        clusterVOs.add(clusterVO);
    }

    private void processDataValue(CObject cObject, String path, String archetypeId, Archetype localAOM, String text, String desc, String type, CObject childCObject) {
        ArchetypeElementVO archetypeElementVO =
                new ArchetypeElementVO(text, desc, type, getIdParentCluster(path, clusterVOs), archetypeId, templateId, path);
        setCardinalities(archetypeElementVO, cObject);
        archetypeElementVOs.add(archetypeElementVO);
        if (OpenEHRDataValues.DV_CODED_TEXT.equals(type)){
            if (codedTextVOs != null){
                loadCodedTexts(archetypeId, localAOM, templateId, path, childCObject, archetypeElementVO.getId(), language, codedTextVOs);
            }
        }else if (OpenEHRDataValues.DV_ORDINAL.equals(type)) {
            if (ordinalVOs != null){
                loadOrdinals(archetypeId, localAOM, templateId, path, childCObject, archetypeElementVO.getId(), language, ordinalVOs);
            }
        }else if (OpenEHRDataValues.DV_QUANTITY.equals(type)) {
            if (unitVOs != null){
                loadUnits(templateId, archetypeElementVO.getId(), childCObject, unitVOs);
            }
        }else if (OpenEHRDataValues.DV_PROPORTION.equals(type)) {
            if (proportionTypeVOs != null){
                loadProportionTypes(templateId, archetypeElementVO.getId(), childCObject, proportionTypeVOs);
            }
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
            if (clusterVO.getPath().length() > length){
                idParentCluster = clusterVO.getId();
                length = clusterVO.getPath().length();
            }
        }
        return idParentCluster;
    }

    private static void loadCodedTexts(
            String archetypdId,
            Archetype ar,
            String templateId,
            String path,
            CObject childCObject,
            String idElement,
            String language,
            Collection<CodedTextVO> codedTextVOs){
        boolean codedListFound = false;
        if (childCObject instanceof CComplexObject){
            List<CAttribute> atts = ((CComplexObject)childCObject).getAttributes();
            if (atts != null){
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
                            if (cCodePhrase.getCodeList() != null){
                                for (String codedStr : cCodePhrase.getCodeList()) {
                                    String text = codedStr;
                                    String desc = codedStr;
                                    CodedTextVO codedText = new CodedTextVO(
                                            text,
                                            desc,
                                            cCodePhrase.getRmTypeName(),
                                            idElement,
                                            archetypdId,
                                            templateId,
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
                if (node == null){
                    Logger.getLogger(GenericObjectBundleManager.class).warn("Terminology code not found '"+codedTextVO.getCode()+"::"+codedTextVO.getTerminology()+"'.");
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
            String archetypdId,
            Archetype ar,
            String templateId,
            String path,
            CObject childCObject,
            String idElement,
            String language,
            Collection<OrdinalVO> ordinalVOs){
        if (childCObject instanceof CDvOrdinal){
            CDvOrdinal cDvOrdinal = (CDvOrdinal)childCObject;
            if (cDvOrdinal.getList() != null){
                for (Ordinal ordinal : cDvOrdinal.getList()) {
                    String codedStr = ordinal.getSymbol().getCodeString();
                    String text = codedStr;
                    String desc = codedStr;
                    if (ordinal.getSymbol().getTerminologyId().getValue().equals("local")){
                        text = getText(ar, codedStr, language);
                        desc = getDescription(ar, codedStr, language);
                    }else{
                        Logger.getLogger(GenericObjectBundleManager.class).error("Unkown terminology: '"+ordinal.getSymbol().getTerminologyId().getValue()+"', skipping...");
                        //TODO TERMINOLOGY SERVICE
                    }
                    ordinalVOs.add(
                            new OrdinalVO(
                                    text,
                                    desc,
                                    cDvOrdinal.getRmTypeName(),
                                    idElement,
                                    archetypdId,
                                    templateId,
                                    path,
                                    ordinal.getValue(),
                                    ordinal.getSymbol().getTerminologyId().getValue(),
                                    ordinal.getSymbol().getCodeString()));
                };
            }
        }
    }

    private static void loadUnits(
            String templateId,
            String idElement,
            CObject childCObject,
            Collection<UnitVO> unitVOs){
        if (childCObject instanceof CDvQuantity){
            CDvQuantity cDvQuantity = (CDvQuantity)childCObject;
            if (cDvQuantity.getList()!=null){
                for (CDvQuantityItem cDvQuantityItem : cDvQuantity.getList()) {
                    unitVOs.add(new UnitVO(templateId, idElement, cDvQuantityItem.getUnits()));
                };
            }
        }
    }

    private static void loadProportionTypes(
            String templateId,
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
                                proportionTypeVOs.add(new ProportionTypeVO(templateId, idElement, proportionType));
                            }
                        }
                    }
                };
            }
        }
    }

    private static String getText(Archetype ar, String codeStr, String language){
        ArchetypeTerm term = getArchetypeTerm(ar, codeStr, language);
        if (term != null){
            return term.getText();
        }else{
            return null;
        }
    }

    private static String getDescription(Archetype ar, String codeStr, String language){
        ArchetypeTerm term = getArchetypeTerm(ar, codeStr, language);
        if (term != null){
            return term.getDescription();
        }else{
            return null;
        }
    }

    private static ArchetypeTerm getArchetypeTerm(Archetype ar, String codeStr, String language){
        ArchetypeTerm term = ar.getOntology().termDefinition(language, codeStr);
        if (term == null){
            term = ar.getOntology().termDefinition(ar.getOriginalLanguage().getCodeString(), codeStr);
        }
        return term;
    }

    public static Archetype getLocalAOM(Archetype currentAOM, String path){
        int i = path.lastIndexOf("[");
        while(i > 0){
            String idArchetype = path.substring(i+1, path.lastIndexOf("]"));
            if (Archetypes.getArchetypeDTO(idArchetype) != null){
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
        if ((i1 > 0) && (i2 > 0)){
            return path.substring(i1, i2);
        }else{
            return null;
        }
    }
}
