package se.cambio.cds.util;

import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.*;


public class QueryPopulationUtil {

    private static final int EHR_QUERY_PAGINATION_NUMBER = 50;

    public static Map<String, Collection<ArchetypeReference>> queryPopulationData(
            Set<String> ehrIdSet, Collection<ArchetypeReference> queryARs) throws PatientNotFoundException, InternalErrorException {
        Map<String, Collection<ArchetypeReference>> ehrData = new HashMap<>();
        Iterator<String> i = ehrIdSet.iterator();
        Collection<String> ehrIdPage = new ArrayList<>();
        while (i.hasNext()) {
            ehrIdPage.add(i.next());
            if (ehrIdPage.size() >= EHR_QUERY_PAGINATION_NUMBER || !i.hasNext()) {
                Map<String, Collection<ArchetypeReference>> ehrDataPage =
                        CDSSessionManager.getEHRFacadeDelegate().queryEHRElements(ehrIdPage, queryARs, null);
                ehrData.putAll(ehrDataPage);
                ehrIdPage.clear();
            }
        }
        return ehrData;
    }
}
