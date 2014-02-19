package se.cambio.cds.model.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.gdl.model.ArchetypeBinding;
import se.cambio.cds.gdl.model.ElementBinding;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.cds.vo.DomainData;
import se.cambio.cds.model.facade.cds.vo.EIValue;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;

import java.util.*;

/**
 * User: iago.corbal
 * Date: 2013-11-26
 * Time: 11:42
 */
public class DomainDataUtil {


    public static Collection<ArchetypeReference> fromDomainData(DomainData ehrData){
        Collection<ArchetypeReference> archetypeReferences = new ArrayList<ArchetypeReference>();
        for (String archetypeId: ehrData.getArdvMap().keySet()){
            Collection<Map<String, EIValue>> dvMaps  = ehrData.getArdvMap().get(archetypeId);
            for (Map<String, EIValue> dvMap : dvMaps){
                ArchetypeReference archetypeReference = new ArchetypeReference(ehrData.getDomainId(), archetypeId, null);
                archetypeReferences.add(archetypeReference);
                for (String elementId : dvMap.keySet()){
                    EIValue eiValue = dvMap.get(elementId);
                    DataValue dv = DataValue.parseValue(eiValue.getDv());
                    if (eiValue.getGuideId()==null){
                        new ElementInstance(elementId, dv, archetypeReference, null, null);
                    }else{
                        if (eiValue.getOperatorKind()==null){
                            new GeneratedElementInstance(elementId, dv, archetypeReference, null, null, eiValue.getGuideId(), eiValue.getGtCode());
                        }else{
                            new PredicateGeneratedElementInstance(elementId, dv, archetypeReference, null, null, eiValue.getGuideId(), eiValue.getGtCode(), eiValue.getOperatorKind());
                        }
                    }
                }
            }
        }
        return archetypeReferences;
    }

    public static DomainData toDomainData(Collection<ArchetypeReference> archetypeReferences){
        String domainId = null;
        Map<String, Collection<Map<String, EIValue>>> ardvMap = new HashMap<String, Collection<Map<String, EIValue>>>();
        for (ArchetypeReference archetypeReference:archetypeReferences){
            if (domainId==null){
                domainId = archetypeReference.getIdDomain();
            }else{
                if (!domainId.equals(archetypeReference.getIdDomain())){
                    Logger.getLogger(DomainDataUtil.class).warn("Storing in the same domain data object two ARs with different domain! ("+domainId+"!="+archetypeReference.getIdDomain()+")");
                }
            }

            Collection <Map<String, EIValue>> dvMaps = ardvMap.get(archetypeReference.getIdArchetype());
            if (dvMaps==null){
                dvMaps = new ArrayList<Map<String, EIValue>>();
                ardvMap.put(archetypeReference.getIdArchetype(), dvMaps);
            }
            Map<String, EIValue> dvMap = new HashMap<String, EIValue>();
            dvMaps.add(dvMap);
            for (ElementInstance elementInstance: archetypeReference.getElementInstancesMap().values()){
                if (elementInstance.getDataValue()!=null){
                    EIValue eiValue = new EIValue(elementInstance.getDataValue().serialise(), null, null, null);
                    if (elementInstance instanceof GeneratedElementInstance){
                        GeneratedElementInstance gei = (GeneratedElementInstance)elementInstance;
                        eiValue.setGuideId(gei.getGuideId());
                        eiValue.setGtCode(gei.getGtCode());
                    }
                    if (elementInstance instanceof PredicateGeneratedElementInstance){
                        eiValue.setOperatorKind(((PredicateGeneratedElementInstance)elementInstance).getOperatorKind());
                    }
                    dvMap.put(elementInstance.getId(), eiValue);
                }
            }
        }
        return new DomainData(domainId,ardvMap);
    }

    public static DomainData generateDomainDataWithGTCodes(Collection<ArchetypeReference> archetypeReferences, Map<String, Guide> guideMap){
        DomainData domainData = DomainDataUtil.toDomainData(archetypeReferences);
        insertGTCodes(domainData, guideMap);
        return domainData;
    }

    public static void insertGTCodes(DomainData domainData, Map<String, Guide> guideMap){
        Map<String, RuleReference> ruleReferenceMap = generateRuleReferencesForEHRElements(guideMap);
        for (Collection<Map<String, EIValue>> ardvCollection : domainData.getArdvMap().values()){
            for(Map<String, EIValue> eiMap: ardvCollection){
                for (String elementId: eiMap.keySet()){
                    EIValue eiValue = eiMap.get(elementId);
                    RuleReference ruleReference = ruleReferenceMap.get(elementId);
                    if (ruleReference!=null){
                        eiValue.setGtCode(ruleReference.getGTCode());
                        eiValue.setGuideId(ruleReference.getGuideId());
                    }else{
                        Logger.getLogger(DomainDataUtil.class).warn("Rule reference not found for elementId '"+elementId+"'.");
                    }
                }
            }
        }
    }


    public static Map<String, RuleReference> generateRuleReferencesForEHRElements(Map<String, Guide> guideMap){
        Map<String, RuleReference> ruleReferenceMap = new HashMap<String, RuleReference>();
        for(Guide guide: guideMap.values()){
            for (ArchetypeBinding archetypeBinding: guide.getDefinition().getArchetypeBindings()){
                if (archetypeBinding.getDomain()==null || Domains.EHR_ID.equals(archetypeBinding.getDomain())){
                    for(ElementBinding elementBinding: archetypeBinding.getElements().values()){
                        String elementId = archetypeBinding.getArchetypeId()+elementBinding.getPath();
                        RuleReference ruleReference = new RuleReference(guide.getId(), elementBinding.getId());
                        ruleReferenceMap.put(elementId, ruleReference);
                    }
                }
            }
        }
        return ruleReferenceMap;
    }


    public static void cleanGTCodes(DomainData domainData){
        for (Collection<Map<String, EIValue>> ardvCollection : domainData.getArdvMap().values()){
            for(Map<String, EIValue> eiMap: ardvCollection){
                for (String elementId: eiMap.keySet()){
                    EIValue eiValue = eiMap.get(elementId);
                    eiValue.setGtCode(null);
                    eiValue.setGuideId(null);
                }
            }
        }
    }
}
