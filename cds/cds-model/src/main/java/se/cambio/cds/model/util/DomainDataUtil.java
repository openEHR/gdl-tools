package se.cambio.cds.model.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.support.terminology.TerminologyService;
import se.cambio.cds.model.facade.cds.vo.DomainData;
import se.cambio.cds.model.facade.cds.vo.EIValue;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstanceBuilder;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2013-11-26
 * Time: 11:42
 */
public class DomainDataUtil {

    public static DvCodedText NULL_FLAVOUR_CODE_NO_INFO = new DvCodedText("no information", new CodePhrase(TerminologyService.OPENEHR, "271"));

    public static List<ArchetypeReference> fromDomainData(DomainData ehrData){
        List<ArchetypeReference> archetypeReferences = new ArrayList<ArchetypeReference>();
        for (String archetypeId: ehrData.getArdvMap().keySet()){
            List<LinkedHashMap<String, EIValue>> dvMaps  = ehrData.getArdvMap().get(archetypeId);
            for (Map<String, EIValue> dvMap : dvMaps){
                ArchetypeReference archetypeReference = new ArchetypeReference(ehrData.getDomainId(), archetypeId, null);
                archetypeReferences.add(archetypeReference);
                for (String elementId : dvMap.keySet()){
                    EIValue eiValue = dvMap.get(elementId);
                    DataValue dv = null;
                    DvCodedText nullFlavourCT = null;
                    if (eiValue.getDv()!=null){
                        dv = DataValue.parseValue(eiValue.getDv());
                    }else{
                        nullFlavourCT = NULL_FLAVOUR_CODE_NO_INFO;
                    }
                    if (eiValue.getRuleReferences()==null || eiValue.getRuleReferences().isEmpty()){
                        new ElementInstance(elementId, dv, archetypeReference, null, nullFlavourCT);
                    }else{
                        GeneratedElementInstance gei = null;
                        if (eiValue.getOperatorKind()==null){
                            gei = new GeneratedElementInstance(elementId, dv, archetypeReference, null, null);

                        }else{
                            gei = new PredicateGeneratedElementInstanceBuilder()
                                    .setId(elementId)
                                    .setDataValue(dv)
                                    .setArchetypeReference(archetypeReference)
                                    .setOperatorKind(eiValue.getOperatorKind())
                                    .createPredicateGeneratedElementInstance();
                        }
                        gei.setRuleReferences(eiValue.getRuleReferences());
                    }
                }
            }
        }
        return archetypeReferences;
    }

    public static DomainData toDomainData(List<ArchetypeReference> archetypeReferences){
        String domainId = null;
        LinkedHashMap<String, List<LinkedHashMap<String, EIValue>>> ardvMap = new LinkedHashMap<String, List<LinkedHashMap<String, EIValue>>>();
        for (ArchetypeReference archetypeReference:archetypeReferences){
            if (domainId==null){
                domainId = archetypeReference.getIdDomain();
            }else{
                if (!domainId.equals(archetypeReference.getIdDomain())){
                    Logger.getLogger(DomainDataUtil.class).warn("Storing in the same domain data object two ARs with different domain! ("+domainId+"!="+archetypeReference.getIdDomain()+")");
                }
            }

            List<LinkedHashMap<String, EIValue>> dvMaps = ardvMap.get(archetypeReference.getIdArchetype());
            if (dvMaps==null){
                dvMaps = new ArrayList<LinkedHashMap<String, EIValue>>();
                ardvMap.put(archetypeReference.getIdArchetype(), dvMaps);
            }
            LinkedHashMap<String, EIValue> dvMap = new LinkedHashMap<String, EIValue>();
            dvMaps.add(dvMap);
            for (ElementInstance elementInstance: archetypeReference.getElementInstancesMap().values()){
                String dvStr = null;
                if (elementInstance.getDataValue()!=null){ //TODO DVStr as null?!?!
                    dvStr = elementInstance.getDataValue().serialise();
                }
                EIValue eiValue = new EIValue(dvStr, null);
                if (elementInstance instanceof GeneratedElementInstance){
                    GeneratedElementInstance gei = (GeneratedElementInstance)elementInstance;
                    eiValue.setRuleReferences(gei.getRuleReferences());
                }
                if (elementInstance instanceof PredicateGeneratedElementInstance){
                    eiValue.setOperatorKind(((PredicateGeneratedElementInstance)elementInstance).getOperatorKind());
                }
                dvMap.put(elementInstance.getId(), eiValue);
            }
        }
        return new DomainData(domainId,ardvMap);
    }
}
