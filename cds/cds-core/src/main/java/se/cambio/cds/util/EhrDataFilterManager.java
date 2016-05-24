package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.instance.ArchetypeReference;

import java.text.DateFormat;
import java.util.*;

@Component
public class EhrDataFilterManager {

    private DateTimeARFinder dateTimeARFinder;
    private PredicateFilterManager predicateFilterManager;

    @Autowired
    public EhrDataFilterManager(DateTimeARFinder dateTimeARFinder, PredicateFilterManager predicateFilterManager) {
        this.dateTimeARFinder = dateTimeARFinder;
        this.predicateFilterManager = predicateFilterManager;
    }

    public Set<ArchetypeReference> filterEHRDataByGuides(String ehrId, DateTime ehrDate, Collection<Guide> guides, Collection<ArchetypeReference> ehrData) {
        Collection<ArchetypeReference> queryARs = new ArrayList<>();
        for (Guide guide : guides) {
            Collection<ArchetypeReference> archetypeReferences = GuideUtil.getArchetypeReferences(guide, ehrDate, true);
            for (ArchetypeReference archetypeReference : archetypeReferences) {
                if (Domains.EHR_ID.equals(archetypeReference.getIdDomain()) || archetypeReference.getIdDomain() == null) {
                    queryARs.add(archetypeReference);
                }
            }
        }
        return filterEHRData(ehrId, ehrDate, queryARs, ehrData);
    }

    public Set<ArchetypeReference> filterEHRData(String ehrId, DateTime ehrDate, Collection<ArchetypeReference> queryARs, Collection<ArchetypeReference> ehrData) {
        Set<ArchetypeReference> ehrIdARs = new HashSet<>();
        if (ehrData == null) {
            Logger.getLogger(EhrDataFilterManager.class).warn("No ehrData found for ehrId '" + ehrId + "'");
        } else {
            for (ArchetypeReference archetypeReference : ehrData) {
                //If using time series comparison filter elements outside the range
                boolean useAR = true;
                if (ehrDate != null) {
                    DateTime dateTime = dateTimeARFinder.getDateTime(archetypeReference);
                    if (dateTime == null || dateTime.isAfter(ehrDate)) {
                        useAR = false;
                        DateFormat df = DateFormat.getDateTimeInstance();
                        if (dateTime == null) {
                            Logger.getLogger(EhrDataFilterManager.class).warn("Date time for ehrId " + ehrId + " with AR " + archetypeReference.getIdArchetype() + " is null!");
                        } else {
                            Logger.getLogger(EhrDataFilterManager.class).debug(df.format(dateTime.toDate()) + " after " + df.format(ehrDate.toDate()));
                        }
                    }
                }
                if (useAR) {
                    ehrIdARs.add(archetypeReference);
                }
            }
            //Fill missing data
            Calendar date = Calendar.getInstance();
            if (ehrDate != null) {
                date = ehrDate.toGregorianCalendar();
            }
            predicateFilterManager.filterByPredicates(queryARs, ehrIdARs, date);
        }
        return ehrIdARs;
    }

    public Set<ArchetypeReference> filterEHRData(DateTime startDateTime, DateTime endDateTime, Collection<ArchetypeReference> ehrData) {
        Set<ArchetypeReference> ehrDataAux = new HashSet<>();
        for (ArchetypeReference archetypeReference : ehrData) {
            boolean useAR = true;
            DateTime dateTime = dateTimeARFinder.getDateTime(archetypeReference);
            if (dateTime == null ||
                    endDateTime != null && dateTime.isAfter(endDateTime) ||
                    startDateTime != null && dateTime.isBefore(startDateTime)) {
                useAR = false;
                if (dateTime == null) {
                    Logger.getLogger(EhrDataFilterManager.class).warn("Date time for Archetype Reference " + archetypeReference.getIdArchetype() + " is null!");
                }
            }
            if (useAR) {
                ehrDataAux.add(archetypeReference);
            }
        }
        return ehrDataAux;
    }
}
