package se.cambio.cds.util;

import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.WindowManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;


public class QueryPopulationUtil {

    private static int EHR_QUERY_PAGINATION_NUMBER = 50;

    public static Map<String, Collection<ArchetypeReference>> queryPopulationData(
            Set<String> ehrIdSet, Collection<ArchetypeReference> queryARs,
            String threadKey, int offset) throws PatientNotFoundException, InternalErrorException {
        Map<String, Collection<ArchetypeReference>> ehrData = new HashMap<String, Collection<ArchetypeReference>>();
        Iterator<String> i = ehrIdSet.iterator();
        double progressIncreaseQuery = (1 / ((double) ehrIdSet.size()));
        Collection<String> ehrIdPage = new ArrayList<String>();
        int ehrCount = 0;
        while (i.hasNext()) {
            ehrIdPage.add(i.next());
            ehrCount++;
            if (ehrIdPage.size() >= EHR_QUERY_PAGINATION_NUMBER || !i.hasNext()) {
                Map<String, Collection<ArchetypeReference>> ehrDataPage =
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
