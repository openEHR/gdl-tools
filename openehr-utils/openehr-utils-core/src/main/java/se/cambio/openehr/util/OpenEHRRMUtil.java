package se.cambio.openehr.util;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.archetype.constraintmodel.CAttribute;
import org.openehr.am.archetype.constraintmodel.CComplexObject;
import org.openehr.am.archetype.constraintmodel.CObject;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ArchetypeElementVOBuilder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class OpenEHRRMUtil {
    public static String EVENT_TIME_PATH = "/data/events/time";
    public static String EXPIRY_TIME_PATH = "/expiry_time";
    public static String NARRATIVE_PATH =  "/narrative";
    public static String TIME_PATH = "/time";
    public static String ISM_TRANSITION_PATH = "/ism_transition/current_state";
    public static String TEMPLATE_ID_PATH = "/archetype_details/template_id";

    private static Collection<String> rmPaths;
    static {
        rmPaths = new ArrayList<String>();
        rmPaths.add(EVENT_TIME_PATH);
        rmPaths.add(EXPIRY_TIME_PATH);
        rmPaths.add(NARRATIVE_PATH);
        rmPaths.add(TIME_PATH);
        rmPaths.add(ISM_TRANSITION_PATH);
        rmPaths.add(TEMPLATE_ID_PATH);
    }
    public static Collection<ArchetypeElementVO> getRMElements(String idArchetype, String idTemplate, String entryType, Archetype ar) {
        return getRMElements(idArchetype, idTemplate, entryType, "", ar);
    }

    public static Collection<ArchetypeElementVO> getRMElements(String idArchetype, String idTemplate, String entryType, String parentPath, Archetype ar) {
        Collection<ArchetypeElementVO> rmArchetypeElements = new ArrayList<ArchetypeElementVO>();
        if (OpenEHRConst.OBSERVATION.equals(entryType)) {
            String eventsTimePath = getEventsTimePath(ar);

            //EventTime
            rmArchetypeElements.add(
                    new ArchetypeElementVOBuilder()
                            .setName(OpenEHRLanguageManager.getMessage("EventTime"))
                            .setDescription(OpenEHRLanguageManager.getMessage("EventTimeDesc"))
                            .setType(OpenEHRDataValues.DV_DATE_TIME)
                            .setIdArchetype(idArchetype).setIdTemplate(idTemplate)
                            .setPath(parentPath + eventsTimePath).createArchetypeElementVO());
        }else if (OpenEHRConst.INSTRUCTION.equals(entryType)){
            //Expiry Time
            rmArchetypeElements.add(
                    new ArchetypeElementVOBuilder()
                            .setName(OpenEHRLanguageManager.getMessage("ExpireTime"))
                            .setDescription(OpenEHRLanguageManager.getMessage("ExpireTimeDesc"))
                            .setType(OpenEHRDataValues.DV_DATE_TIME)
                            .setIdArchetype(idArchetype)
                            .setIdTemplate(idTemplate)
                            .setPath(parentPath + EXPIRY_TIME_PATH).createArchetypeElementVO());
            //Narrative Description
            rmArchetypeElements.add(
                    new ArchetypeElementVOBuilder()
                            .setName(OpenEHRLanguageManager.getMessage("NarrativeDescription"))
                            .setDescription(OpenEHRLanguageManager.getMessage("NarrativeDescriptionDesc"))
                            .setType(OpenEHRDataValues.DV_TEXT)
                            .setIdArchetype(idArchetype)
                            .setIdTemplate(idTemplate)
                            .setPath(parentPath + NARRATIVE_PATH)
                            .createArchetypeElementVO());
        }else if (OpenEHRConst.ACTION.equals(entryType)){
            //Date and time Action step performed
            rmArchetypeElements.add(
                    new ArchetypeElementVOBuilder()
                            .setName(OpenEHRLanguageManager.getMessage("DateTimeActionPerformed"))
                            .setDescription(OpenEHRLanguageManager.getMessage("DateTimeActionPerformedDesc"))
                            .setType(OpenEHRDataValues.DV_DATE_TIME)
                            .setIdArchetype(idArchetype)
                            .setIdTemplate(idTemplate)
                            .setPath(parentPath + TIME_PATH)
                            .createArchetypeElementVO());
            //Current Action State
            rmArchetypeElements.add(
                    new ArchetypeElementVOBuilder()
                            .setName(OpenEHRLanguageManager.getMessage("CurrentActionState"))
                            .setDescription(OpenEHRLanguageManager.getMessage("CurrentActionStateDesc"))
                            .setType(OpenEHRDataValues.DV_CODED_TEXT)
                            .setIdArchetype(idArchetype)
                            .setIdTemplate(idTemplate)
                            .setPath(parentPath + ISM_TRANSITION_PATH)
                            .createArchetypeElementVO());
        }
        if (parentPath.isEmpty()) { //TODO Check if this assumption is correct
            //Template Id
            rmArchetypeElements.add(
                    new ArchetypeElementVOBuilder()
                            .setName(OpenEHRLanguageManager.getMessage("TemplateId"))
                            .setDescription(OpenEHRLanguageManager.getMessage("TemplateIdDesc"))
                            .setType(OpenEHRDataValues.DV_TEXT)
                            .setIdArchetype(idArchetype)
                            .setIdTemplate(idTemplate)
                            .setPath(TEMPLATE_ID_PATH)
                            .createArchetypeElementVO());
        }
        return rmArchetypeElements;
    }

    private static String getEventsTimePath(Archetype ar) {
        if (ar == null || ar.getDefinition() == null) {
            return EVENT_TIME_PATH;
        }
        CAttribute data = ar.getDefinition().getAttribute("data");
        if (data == null) {
            return EVENT_TIME_PATH;
        }
        List<CObject> dataItems = data.getChildren();
        if (dataItems.isEmpty()){
            return EVENT_TIME_PATH;
        }
        CComplexObject firstDataNode = (CComplexObject) dataItems.iterator().next();
        CAttribute events = firstDataNode.getAttribute("events");
        if (events == null) {
            return EVENT_TIME_PATH;
        }
        List<CObject> eventItems = events.getChildren();
        if (eventItems.isEmpty()){
            return EVENT_TIME_PATH;
        }
        CObject cObject = eventItems.iterator().next();
        return cObject.path() + "/time";
    }

    public final static Collection<String> getRmPaths(){
        return rmPaths;
    }

    public static boolean isRMPath(String path) {
        Iterator<String> i = rmPaths.iterator();
        while(i.hasNext()){
            if (path.endsWith(i.next())){
                return true;
            }
        }
        return false;
    }
}
