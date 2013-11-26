package se.cambio.cds.model.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.model.facade.cds.vo.DomainData;
import se.cambio.cds.model.facade.cds.vo.EIValue;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

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
                    if (eiValue.getGtCode()==null){
                        new ElementInstance(elementId, DataValue.parseValue(eiValue.getDv()), archetypeReference, null, null);
                    }else{
                        new GeneratedElementInstance(elementId, DataValue.parseValue(eiValue.getDv()), archetypeReference, null, null, eiValue.getGuideId(), eiValue.getGtCode());
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
                    EIValue eiValue = new EIValue(elementInstance.getDataValue().serialise(), null, null);
                    if (elementInstance instanceof GeneratedElementInstance){
                        GeneratedElementInstance gei = (GeneratedElementInstance)elementInstance;
                        eiValue.setGuideId(gei.getGuideId());
                        eiValue.setGtCode(gei.getGtCode());
                    }
                    dvMap.put(elementInstance.getId(), eiValue);
                }
            }
        }
        return new DomainData(domainId,ardvMap);
    }

}
