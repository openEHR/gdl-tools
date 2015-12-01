package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import se.cambio.cds.model.instance.ArchetypeReference;

import java.text.DateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

@Component
public class EHRDataFilterUtil {

    DateTimeARFinder dateTimeARFinder;

    @Autowired
    public EHRDataFilterUtil(DateTimeARFinder dateTimeARFinder) {
        this.dateTimeARFinder = dateTimeARFinder;
    }

    public Set<ArchetypeReference> filterEHRData(String ehrId, DateTime ehrDate, Collection<ArchetypeReference> queryARs, Collection<ArchetypeReference> ehrData) {
        Set<ArchetypeReference> ehrIdARs = new HashSet<ArchetypeReference>();
        if (ehrData == null) {
            Logger.getLogger(EHRDataFilterUtil.class).warn("No ehrData found for ehrId '" + ehrId + "'");
        } else {
            for (ArchetypeReference archetypeReference : ehrData) {
                //If using time series comparison filter elements outside the range
                boolean useAR = true;
                if (ehrDate != null) {
                    DateTime dateTime = dateTimeARFinder.getDateTime(archetypeReference);
                    //DateFormat df2 = DateFormat.getDateTimeInstance();
                    //Logger.getLogger(DashboardFetchPageExecutionThread.class).debug("Comparing '"+ehrId+"' with ehr date '"+df2.format(ehrDate.toDate())+"' to date '"+(dateTime==null?"null":df2.format(dateTime.toDate()))+"' eiId = '"+elementInstance.getId()+"'");
                    if (dateTime == null || dateTime.isAfter(ehrDate)) {
                        useAR = false;
                        DateFormat df = DateFormat.getDateTimeInstance();
                        if (dateTime == null) {
                            Logger.getLogger(EHRDataFilterUtil.class).warn("Date time for ehrId " + ehrId + " with AR " + archetypeReference.getIdArchetype() + " is null!");
                        } else {
                            Logger.getLogger(EHRDataFilterUtil.class).debug(df.format(dateTime.toDate()) + " after " + df.format(ehrDate.toDate()));
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
            PredicateFilterUtil.filterByPredicates(queryARs, ehrIdARs, date);
        }
        return ehrIdARs;
    }
}
