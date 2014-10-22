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
                ehrData.putAll(ehrDataPage);
                ehrIdPage.clear();
                WindowManager.setCurrentProgress(
                        threadKey, threadKey + " (offset=" + offset + ") : Loading (" + ehrCount + "/" + ehrIdSet.size() + ")" + "...",
                        progressIncreaseQuery * ehrCount);
            }
        }
        return ehrData;
    }
}
