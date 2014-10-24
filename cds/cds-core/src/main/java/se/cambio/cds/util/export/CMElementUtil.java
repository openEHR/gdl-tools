package se.cambio.cds.util.export;

import org.openehr.am.archetype.Archetype;
import org.openehr.am.serialize.ADLSerializer;
import se.cambio.cds.controller.guide.GuideUtil;
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
import se.cambio.cm.model.util.CMElement;
import se.cambio.cm.model.view.dto.DSViewDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;

public class CMElementUtil {

    public static Object convert(CMElement cmElement, ArchetypeManager archetypeManager) throws InternalErrorException {
        if (cmElement instanceof ArchetypeDTO) {
            return archetypeManager.getArchetypes().getArchetypeAOM((ArchetypeDTO) cmElement);
        } else if (cmElement instanceof TemplateDTO) {
            return archetypeManager.getTemplates().getTemplateAOM((TemplateDTO) cmElement);
        } else if (cmElement instanceof GuideDTO) {
            return Guides.getInstance().getGuide((GuideDTO) cmElement);
        } else if (cmElement instanceof TerminologyDTO) {
            return cmElement.getSource();
        } else if (cmElement instanceof StudyDTO) {
            return JSONSerialization.parse(Study.class, cmElement.getSource());
        } else if (cmElement instanceof DSViewDTO) {
            InputStream is = new ByteArrayInputStream(cmElement.getSource().getBytes());
            return new DSViewParser().parseDSView(is);
        } else if (cmElement instanceof KBInstanceDTO) {
            return JSONSerialization.parse(KBInstance.class, cmElement.getSource());
        } else if (cmElement instanceof OrderSetDTO) {
            return JSONSerialization.parse(OrderSet.class, cmElement.getSource());
        } else if (cmElement instanceof CDSAppDTO) {
            return JSONSerialization.parse(CDSApp.class, cmElement.getSource());
        } else if (cmElement instanceof ScenarioDTO) {
            return JSONSerialization.parse(Scenario.class, cmElement.getSource());
        } else {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot parse cm element '" + cmElement.getClass().getName() + "'"));
        }
    }

    public static void fill(CMElement cmElement, Object object) throws InternalErrorException {
        if (object instanceof Archetype) {
            try {
                Archetype archetype = (Archetype) object;
                String archetypeSrc = new ADLSerializer().output(archetype);
                cmElement.setId(archetype.getArchetypeId().getValue());
                cmElement.setSource(archetypeSrc);
            } catch (IOException e) {
                throw new InternalErrorException(e);
            }
        } else if (object instanceof Guide) {
            Guide guide = (Guide) object;
            try {
                String source = GuideUtil.serializeGuide(guide);
                cmElement.setId(guide.getId());
                cmElement.setSource(source);
            } catch (Exception e) {
                throw new InternalErrorException(e);
            }
        } else if (object instanceof Study) {
            Study study = (Study) object;
            cmElement.setId(study.getStudyId());
            cmElement.setSource(JSONSerialization.serialize(Study.class, study));
        } else if (object instanceof DecisionSupportViewBundle) {
            DecisionSupportViewBundle decisionSupportViewBundle = (DecisionSupportViewBundle) object;
            cmElement.setId(decisionSupportViewBundle.getDecisionSupportView().getDsViewId());
            cmElement.setSource(decisionSupportViewBundle.getDsvSrc());
        } else if (object instanceof KBInstance) {
            KBInstance kbInstance = (KBInstance) object;
            cmElement.setId(kbInstance.getKbiId());
            cmElement.setSource(JSONSerialization.serialize(KBInstance.class, kbInstance));
        } else if (object instanceof OrderSet) {
            OrderSet orderSet = (OrderSet) object;
            cmElement.setId(orderSet.getOrderSetId());
            cmElement.setSource(JSONSerialization.serialize(OrderSet.class, orderSet));
        } else if (object instanceof CDSApp) {
            CDSApp cdsApp = (CDSApp) object;
            cmElement.setId(cdsApp.getCdsAppId());
            cmElement.setSource(JSONSerialization.serialize(CDSApp.class, cdsApp));
        } else if (object instanceof Scenario) {
            Scenario scenario = (Scenario) object;
            cmElement.setId(scenario.getScenarioId());
            cmElement.setSource(JSONSerialization.serialize(Scenario.class, scenario));
        } else {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot parse cm element '" + object.getClass().getName() + "'"));
        }
        cmElement.setLastUpdate(Calendar.getInstance().getTime());
    }
}
