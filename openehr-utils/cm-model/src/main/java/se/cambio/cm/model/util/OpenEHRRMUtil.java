package se.cambio.cm.model.util;

import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;
import se.cambio.cm.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.OpenEHRConst;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.OpenEHRLanguageManager;

import java.util.ArrayList;
import java.util.Collection;

public class OpenEHRRMUtil {
    public static final String EVENT_TIME_PATH = "/data/events/time";
    public static final String EXPIRY_TIME_PATH = "/expiry_time";
    public static final String NARRATIVE_PATH = "/narrative";
    public static final String TIME_PATH = "/time";
    public static final String TIMING_PATH = "/activities/timing";
    public static final String ACTIVITIES_PATH = "/activities";
    public static final String ISM_TRANSITION_PATH = "/ism_transition/current_state";
    public static final String TEMPLATE_ID_PATH = "/archetype_details/template_id";
    public static final String ARCHETYPE_DETAILS_PATH = "/archetype_details";

    private static Collection<String> rmPaths;

    static {
        rmPaths = new ArrayList<>();
        rmPaths.add(EVENT_TIME_PATH);
        rmPaths.add(EXPIRY_TIME_PATH);
        rmPaths.add(NARRATIVE_PATH);
        rmPaths.add(TIME_PATH);
        rmPaths.add(ISM_TRANSITION_PATH);
        rmPaths.add(TEMPLATE_ID_PATH);
    }

    public static Collection<ArchetypeElementVO> getRMElements(String idArchetype, String idTemplate, String entryType) {
        return getRMElements(idArchetype, idTemplate, entryType, "");
    }

    public static Collection<ArchetypeElementVO> getRMElements(String idArchetype, String idTemplate, String entryType, String parentPath) {
        Collection<ArchetypeElementVO> rmArchetypeElements = new ArrayList<>();
        if (OpenEHRConst.OBSERVATION.equals(entryType)) {
            String eventsTimePath = EVENT_TIME_PATH;
            //EventTime
            rmArchetypeElements.add(
                    ArchetypeElementVO.builder()
                            .name(OpenEHRLanguageManager.getMessage("EventTime"))
                            .description(OpenEHRLanguageManager.getMessage("EventTimeDesc"))
                            .type(OpenEHRDataValues.DV_DATE_TIME)
                            .idArchetype(idArchetype)
                            .idTemplate(idTemplate)
                            .lowerCardinality(1)
                            .upperCardinality(1)
                            .path(parentPath + eventsTimePath)
                            .build());
        } else if (OpenEHRConst.INSTRUCTION.equals(entryType)) {
            //Expiry Time
            rmArchetypeElements.add(
                    ArchetypeElementVO.builder()
                            .name(OpenEHRLanguageManager.getMessage("ExpireTime"))
                            .description(OpenEHRLanguageManager.getMessage("ExpireTimeDesc"))
                            .type(OpenEHRDataValues.DV_DATE_TIME)
                            .idArchetype(idArchetype)
                            .idTemplate(idTemplate)
                            .lowerCardinality(0)
                            .upperCardinality(1)
                            .path(parentPath + EXPIRY_TIME_PATH)
                            .build());
            //Narrative Description
            rmArchetypeElements.add(
                    ArchetypeElementVO.builder()
                            .name(OpenEHRLanguageManager.getMessage("NarrativeDescription"))
                            .description(OpenEHRLanguageManager.getMessage("NarrativeDescriptionDesc"))
                            .type(OpenEHRDataValues.DV_TEXT)
                            .idArchetype(idArchetype)
                            .idTemplate(idTemplate)
                            .path(parentPath + NARRATIVE_PATH)
                            .lowerCardinality(1)
                            .upperCardinality(1)
                            .build());
            //Detailed activity timing
            rmArchetypeElements.add(
                    ArchetypeElementVO.builder()
                            .name(OpenEHRLanguageManager.getMessage("DetailedActivityTiming"))
                            .description(OpenEHRLanguageManager.getMessage("DetailedActivityTimingDesc"))
                            .type(OpenEHRDataValues.DV_PARSABLE)
                            .idArchetype(idArchetype)
                            .idTemplate(idTemplate)
                            .path(parentPath + TIMING_PATH)
                            .lowerCardinality(1)
                            .upperCardinality(1)
                            .build());
        } else if (OpenEHRConst.ACTION.equals(entryType)) {
            //Date and time Action step performed
            rmArchetypeElements.add(
                    ArchetypeElementVO.builder()
                            .name(OpenEHRLanguageManager.getMessage("DateTimeActionPerformed"))
                            .description(OpenEHRLanguageManager.getMessage("DateTimeActionPerformedDesc"))
                            .type(OpenEHRDataValues.DV_DATE_TIME)
                            .idArchetype(idArchetype)
                            .idTemplate(idTemplate)
                            .path(parentPath + TIME_PATH)
                            .lowerCardinality(1)
                            .upperCardinality(1)
                            .build());
            //Current Action State
            rmArchetypeElements.add(
                    ArchetypeElementVO.builder()
                            .name(OpenEHRLanguageManager.getMessage("CurrentActionState"))
                            .description(OpenEHRLanguageManager.getMessage("CurrentActionStateDesc"))
                            .type(OpenEHRDataValues.DV_CODED_TEXT)
                            .idArchetype(idArchetype)
                            .idTemplate(idTemplate)
                            .path(parentPath + ISM_TRANSITION_PATH)
                            .lowerCardinality(1)
                            .upperCardinality(1)
                            .build());
        }
        if (parentPath.isEmpty()) { //TODO Check if this assumption is correct
            //Template Id
            rmArchetypeElements.add(
                    ArchetypeElementVO.builder()
                            .name(OpenEHRLanguageManager.getMessage("TemplateId"))
                            .description(OpenEHRLanguageManager.getMessage("TemplateIdDesc"))
                            .type(OpenEHRDataValues.DV_TEXT)
                            .idArchetype(idArchetype)
                            .idTemplate(idTemplate)
                            .path(TEMPLATE_ID_PATH)
                            .lowerCardinality(0)
                            .upperCardinality(1)
                            .build());
        }
        return rmArchetypeElements;
    }

    public static Collection<ClusterVO> getRMClusters(String idArchetype, String idTemplate, String entryType) {
        Collection<ClusterVO> rmArchetypeClusters = new ArrayList<>();
        ClusterVO clusterVO = ClusterVO.builder()
                .name(OpenEHRLanguageManager.getMessage("ArchetypeDetails"))
                .description(OpenEHRLanguageManager.getMessage("ArchetypeDetails"))
                .type(OpenEHRConst.CLUSTER)
                .idArchetype(idArchetype)
                .idTemplate(idTemplate)
                .path(ARCHETYPE_DETAILS_PATH)
                .build();
        rmArchetypeClusters.add(clusterVO);
        if (OpenEHRConst.INSTRUCTION.equals(entryType)) {
            clusterVO = ClusterVO.builder()
                    .name(OpenEHRLanguageManager.getMessage("Activity"))
                    .description(OpenEHRLanguageManager.getMessage("ActivityDesc"))
                    .type(OpenEHRConst.CLUSTER)
                    .idArchetype(idArchetype)
                    .idTemplate(idTemplate)
                    .path(ACTIVITIES_PATH)
                    .build();
            rmArchetypeClusters.add(clusterVO);
        }
        return rmArchetypeClusters;
    }

    public static Collection<String> getRmPaths() {
        return rmPaths;
    }

    public static boolean isRMPath(String path) {
        for (String rmPath : rmPaths) {
            if (path.endsWith(rmPath)) {
                return true;
            }
        }
        return false;
    }
}
