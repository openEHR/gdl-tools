package se.cambio.cds.util;

import org.openehr.rm.datatypes.quantity.DvQuantity;
import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.util.WindowManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.*;

/**
 * User: Iago.Corbal
 * Date: 2014-05-28
 * Time: 16:26
 */
public class QueryPopulationUtil {

    private static int EHR_QUERY_PAGINATION_NUMBER = 50;

    public static Map<String, Collection<ElementInstance>> queryPopulationData(Set<String> ehrIdSet, Collection<ArchetypeReference> queryARs, String threadKey, int offset) throws PatientNotFoundException, InternalErrorException {
        Map<String, Collection<ElementInstance>> ehrData = new HashMap<String, Collection<ElementInstance>>();
        Iterator<String> i = ehrIdSet.iterator();
        double progressIncreaseQuery = (1/((double)ehrIdSet.size()));
        Collection<String> ehrIdPage = new ArrayList<String>();
        int ehrCount = 0;
        while(i.hasNext()){
            ehrIdPage.add(i.next());
            ehrCount++;
            if (ehrIdPage.size()>=EHR_QUERY_PAGINATION_NUMBER || !i.hasNext()){
                Map<String, Collection<ElementInstance>> ehrDataPage =
                        CDSSessionManager.getEHRFacadeDelegate().queryEHRElements(ehrIdPage, queryARs, null);
                filterEHRDataPageForSepsis(ehrDataPage);
                ehrData.putAll(ehrDataPage);
                ehrIdPage.clear();
                WindowManager.setCurrentProgress(
                        threadKey, threadKey + " (offset=" + offset + ") : Loading (" + ehrCount + "/" + ehrIdSet.size() + ")" + "...",
                        progressIncreaseQuery * ehrCount);
            }
        }
        return ehrData;
    }

    //TODO REMOVE VVVVVVV

    private static void filterEHRDataPageForSepsis(Map<String, Collection<ElementInstance>> ehrDataPage){
        Map<String, Collection<ElementInstance>> ehrDataPageAux = new HashMap<String, Collection<ElementInstance>>();
        long totalNumEI = 0;
        long filteredNumEI = 0;
        for (String ehrId:ehrDataPage.keySet()){
            Collection<ElementInstance> eis = ehrDataPage.get(ehrId);
            int numEIS = eis.size();
            totalNumEI+=numEIS;
            eis = filterDataForSepsisTest(eis);
            filteredNumEI+=(numEIS-eis.size());
            ehrDataPage.put(ehrId, eis);
        }
        //System.out.println("Avg num EIs = "+(totalNumEI/ehrDataPage.keySet().size()+", Avg num EIs filtered = "+(filteredNumEI/ehrDataPage.keySet().size())));
    }


    private static Collection<ElementInstance> filterDataForSepsisTest(Collection<ElementInstance> elementInstances){
        Collection<ElementInstance> elementInstancesAux = new ArrayList<ElementInstance>();
        HashSet<ArchetypeReference> filteredARs = new HashSet<ArchetypeReference>();
        for (ElementInstance elementInstance : elementInstances){
            if (elementInstance.getId().equals("openEHR-EHR-OBSERVATION.body_temperature.v1/data[at0002]/events[at0003]/data[at0001]/items[at0004]")){
                DvQuantity dvQuantity = (DvQuantity)elementInstance.getDataValue();
                if (dvQuantity.getMagnitude()>=36.0 && dvQuantity.getMagnitude()<=38.0){
                    filteredARs.add(elementInstance.getArchetypeReference());
                }
            } else if (elementInstance.getId().equals("openEHR-EHR-OBSERVATION.lab_test-full_blood_count.v1/data[at0001]/events[at0002]/data[at0003]/items[at0078.13]")){
                DvQuantity dvQuantity = (DvQuantity)elementInstance.getDataValue();
                if (dvQuantity.getMagnitude()>=4 && dvQuantity.getMagnitude()<=12){
                    filteredARs.add(elementInstance.getArchetypeReference());
                }
            }
        }
        for (ElementInstance elementInstance : elementInstances){
            if (!filteredARs.contains(elementInstance.getArchetypeReference())){
                elementInstancesAux.add(elementInstance);
            }
        }
        return elementInstancesAux;
    }
}
