package se.cambio.openehr.util;

import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 * User: Iago.Corbal
 * Date: 2014-07-14
 * Time: 15:25
 */
public class OpenEHRRMUtil {
    public static String EVENT_TIME_PATH = "/data/events/time";
    public static String EXPIRY_TIME_PATH = "/expiry_time";
    public static String NARRATIVE_PATH =  "/narrative";
    public static String TIME_PATH = "/time";
    public static String ISM_TRANSITION_PATH = "/ism_transition/current_state";
    public static String TEMPLATE_ID_PATH = "/archetype_details/template_id";

    private static Collection<String> rmPaths;
    static{
        rmPaths = new ArrayList<String>();
        rmPaths.add(EVENT_TIME_PATH);
        rmPaths.add(EXPIRY_TIME_PATH);
        rmPaths.add(NARRATIVE_PATH);
        rmPaths.add(TIME_PATH);
        rmPaths.add(ISM_TRANSITION_PATH);
        rmPaths.add(TEMPLATE_ID_PATH);
    }

    public static Collection<ArchetypeElementVO> getRMElements(String idArchetype, String idTemplate, String entryType){
        Collection<ArchetypeElementVO> rmArchetypeElements = new ArrayList<ArchetypeElementVO>();
        if (OpenEHRConst.OBSERVATION.equals(entryType)){
	    /*Origin (Use EventTime instead)
	    archetypeElementVOs.add(
		    new ArchetypeElementVO(
			    LanguageManager.getMessage("Origin"),
			    LanguageManager.getMessage("OriginDesc"),
			    OpenEHRDataValues.DV_DATE_TIME, null,
			    idArchetype, "/time"));
	     */
            //EventTime
            rmArchetypeElements.add(
                    new ArchetypeElementVO(
                            OpenEHRLanguageManager.getMessage("EventTime"),
                            OpenEHRLanguageManager.getMessage("EventTimeDesc"),
                            OpenEHRDataValues.DV_DATE_TIME, null,
                            idArchetype, idTemplate, EVENT_TIME_PATH));
        }else if (OpenEHRConst.INSTRUCTION.equals(entryType)){
            //Expiry Time
            rmArchetypeElements.add(
                    new ArchetypeElementVO(
                            OpenEHRLanguageManager.getMessage("ExpireTime"),
                            OpenEHRLanguageManager.getMessage("ExpireTimeDesc"),
                            OpenEHRDataValues.DV_DATE_TIME, null,
                            idArchetype, idTemplate, EXPIRY_TIME_PATH));
            //Narrative Description
            rmArchetypeElements.add(
                    new ArchetypeElementVO(
                            OpenEHRLanguageManager.getMessage("NarrativeDescription"),
                            OpenEHRLanguageManager.getMessage("NarrativeDescriptionDesc"),
                            OpenEHRDataValues.DV_TEXT, null,
                            idArchetype, idTemplate,NARRATIVE_PATH));
        }else if (OpenEHRConst.ACTION.equals(entryType)){
            //Date and time Action step performed
            rmArchetypeElements.add(
                    new ArchetypeElementVO(
                            OpenEHRLanguageManager.getMessage("DateTimeActionPerformed"),
                            OpenEHRLanguageManager.getMessage("DateTimeActionPerformedDesc"),
                            OpenEHRDataValues.DV_DATE_TIME, null,
                            idArchetype, idTemplate, TIME_PATH));
            //Current Action State
            rmArchetypeElements.add(
                    new ArchetypeElementVO(
                            OpenEHRLanguageManager.getMessage("CurrentActionState"),
                            OpenEHRLanguageManager.getMessage("CurrentActionStateDesc"),
                            OpenEHRDataValues.DV_CODED_TEXT, null,
                            idArchetype, idTemplate, ISM_TRANSITION_PATH));
        }else if (OpenEHRConst.EVALUATION.equals(entryType)){

        }
        //Template Id
        rmArchetypeElements.add(
                new ArchetypeElementVO(
                        OpenEHRLanguageManager.getMessage("TemplateId"),
                        OpenEHRLanguageManager.getMessage("TemplateIdDesc"),
                        OpenEHRDataValues.DV_TEXT, null,
                        idArchetype, idTemplate, TEMPLATE_ID_PATH));
        return rmArchetypeElements;
    }

    public final static Collection<String> getRmPaths(){
        return rmPaths;
    }

    public static boolean isRMPath(String path){
        Iterator<String> i = rmPaths.iterator();
        while(i.hasNext()){
            if (path.endsWith(i.next())){
                return true;
            }
        }
        return false;
    }
}
