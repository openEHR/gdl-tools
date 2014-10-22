package se.cambio.cds.util;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.guide.SimpleGuideManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.cds.vo.DomainData;
import se.cambio.cds.model.facade.cds.vo.EIValue;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.model.util.DomainDataUtil;

import java.util.*;

/**
 * User: iago.corbal
 * Date: 2014-02-21
 * Time: 11:52
 */
public class DomainDataGTCodeUtil {

    public static DomainData generateDomainDataWithGTCodes(List<ArchetypeReference> archetypeReferences, Map<String, Guide> guideMap, Calendar date){
        DomainData domainData = DomainDataUtil.toDomainData(archetypeReferences);
        insertGTCodes(domainData, guideMap, date);
        return domainData;
    }

    public static void insertGTCodes(DomainData domainData, Map<String, Guide> guideMap, Calendar date){
        SimpleGuideManager simpleGuideManager = new SimpleGuideManager(guideMap.values());
        GeneratedElementInstanceCollection geic = simpleGuideManager.getCompleteElementInstanceCollection();
        List<ArchetypeReference> ars = DomainDataUtil.fromDomainData(domainData);
        Iterator<ArchetypeReference> i = ars.iterator();
        for (List<LinkedHashMap<String, EIValue>> ardvCollection : domainData.getArdvMap().values()){
            for(Map<String, EIValue> eiMap: ardvCollection){
                ArchetypeReference ar = i.next();
                Iterator<ArchetypeReference> arsI = geic.getArchetypeReferences(ar).iterator();
                boolean fullMatch = false;
                while(arsI.hasNext()){
                    ArchetypeReference arAux = arsI.next();
                    if (arAux instanceof GeneratedArchetypeReference){
                        GeneratedArchetypeReference gar = (GeneratedArchetypeReference)arAux;
                        if (ElementInstanceCollectionUtil.matches(gar, ar, guideMap, date)){
                            fullMatch = true;
                            for (String elementId: eiMap.keySet()){
                                EIValue eiValue = eiMap.get(elementId);
                                ElementInstance elementInstance = gar.getElementInstancesMap().get(elementId);
                                if (elementInstance instanceof GeneratedElementInstance){
                                    GeneratedElementInstance gei = (GeneratedElementInstance) elementInstance;
                                    eiValue.getRuleReferences().addAll(gei.getRuleReferences());
                                }else{
                                    fullMatch = false;
                                }
                            }
                        }
                    }
                }
                if (!fullMatch){
                    Logger.getLogger(DomainDataGTCodeUtil.class).warn("No GT codes found for Archetype Reference '"+ar.getIdArchetype()+"'.");
                }
            }
        }
    }

    public static void cleanGTCodes(DomainData domainData){
        for (List<LinkedHashMap<String, EIValue>> ardvCollection : domainData.getArdvMap().values()){
            for(LinkedHashMap<String, EIValue> eiMap: ardvCollection){
                for (String elementId: eiMap.keySet()){
                    EIValue eiValue = eiMap.get(elementId);
                    eiValue.getRuleReferences().clear();
                }
            }
        }
    }
}
