package se.cambio.cds.util;

import org.joda.time.DateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cm.model.util.OpenEHRRMUtil;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.util.OpenEHRConst;


public class DateTimeARFinder {

    private Logger logger = LoggerFactory.getLogger(DateTimeARFinder.class);

    private Environment environment;

    public DateTimeARFinder(Environment environment) {
        this.environment = environment;
    }

    public DateTime getDateTime(ArchetypeReference ar) {
        String dvDateTimePath = getEventTimePath(ar.getIdArchetype());
        if (dvDateTimePath != null) {
            ElementInstance ei = ar.getElementInstancesMap().get(ar.getIdArchetype() + dvDateTimePath);
            return getDateTime(ei);
        } else {
            return null;
        }
    }

    public String getEventTimePath(String archetypeId) {
        String rmName = Archetypes.getEntryType(archetypeId);
        if (OpenEHRConst.OBSERVATION.equals(rmName)) {
            return OpenEHRRMUtil.EVENT_TIME_PATH;
        } else if (OpenEHRConst.ACTION.equals(rmName)) {
            return OpenEHRRMUtil.TIME_PATH;
        } else {
            if (OpenEHRConst.EVALUATION.equals(rmName) ||
                    OpenEHRConst.INSTRUCTION.equals(rmName)) {
                String dateTimePath = environment.getProperty(archetypeId);
                if (dateTimePath == null) {
                    logger.warn("Unregistered DvDateTime for '" + archetypeId + "', please add the path to 'date-time-path.properties'");
                }
                return dateTimePath;
            } else {
                logger.warn("Unknown RM '" + rmName + "'");
                return null;
            }
        }
    }

    private DateTime getDateTime(ElementInstance ei) {
        if (ei != null) {
            if (ei.getDataValue() instanceof DvDateTime) {
                DvDateTime dvDateTime = ((DvDateTime) ei.getDataValue());
                if (dvDateTime.getDateTime() != null) {
                    return dvDateTime.getDateTime();
                } else {
                    logger.warn("Element instance '" + ei.getId() + "' has no DVDateTime.");
                }
            } else {
                String dvType = ei.getDataValue() == null ? "null" : ei.getDataValue().getClass().getSimpleName();
                logger.warn("Element instance '" + ei.getId() + "' data value is not DVDateTime, '" + dvType + "' found instead.)");
            }
        } else {
            logger.debug("Element instance null");
        }
        return null;
    }
}
