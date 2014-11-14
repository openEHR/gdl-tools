package se.cambio.cds.util.export;

import org.openehr.am.archetype.Archetype;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.app.CDSApp;
import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.cds.model.orderset.OrderSet;
import se.cambio.cds.model.scenario.Scenario;
import se.cambio.cds.model.study.Study;
import se.cambio.cds.model.view.DecisionSupportViewBundle;
import se.cambio.cds.util.DSViewParser;
import se.cambio.cds.util.export.json.JSONSerialization;
import se.cambio.cm.model.app.dto.CDSAppDTO;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.cm.model.orderset.dto.OrderSetDTO;
import se.cambio.cm.model.scenario.dto.ScenarioDTO;
import se.cambio.cm.model.study.dto.StudyDTO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.cm.model.view.dto.DSViewDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

public class CMElementUtil {

    public static Archetype convert(ArchetypeDTO archetypeDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return archetypeManager.getArchetypes().getArchetypeAOM(archetypeDTO);
    }

    public static Archetype convert(TemplateDTO templateDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return archetypeManager.getTemplates().getTemplateAOM(templateDTO);
    }

    public static Guide convert(GuideDTO guideDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return Guides.getInstance().getGuide(guideDTO);
    }

    public static String convert(TerminologyDTO terminologyDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return terminologyDTO.getSource();
    }

    public static Study convert(StudyDTO studyDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return JSONSerialization.parse(Study.class, studyDTO.getSource());
    }

    public static DecisionSupportViewBundle convert(DSViewDTO dsViewDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        InputStream is = new ByteArrayInputStream(dsViewDTO.getSource().getBytes());
        return new DSViewParser().parseDSView(is);
    }

    public static KBInstance convert(KBInstanceDTO kbInstanceDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return JSONSerialization.parse(KBInstance.class, kbInstanceDTO.getSource());
    }

    public static OrderSet convert(OrderSetDTO orderSetDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return JSONSerialization.parse(OrderSet.class, orderSetDTO.getSource());
    }

    public static CDSApp convert(CDSAppDTO cdsAppDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return JSONSerialization.parse(CDSApp.class, cdsAppDTO.getSource());
    }

    public static Scenario convert(ScenarioDTO scenarioDTO, ArchetypeManager archetypeManager) throws InternalErrorException {
        return JSONSerialization.parse(Scenario.class, scenarioDTO.getSource());
    }
}
