package se.cambio.openehr.controller;

import openEHR.v1.template.TEMPLATE;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.SerializationUtils;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.template.OETParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.TemplateFlattener;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.Map;

import static java.lang.String.format;

public class TemplateObjectBundleManager {

    private TemplateDTO templateDTO = null;
    private final Map<String, Archetype> archetypeMap;
    private TerminologyService terminologyService;
    private UserConfigurationManager userConfigurationManager;
    private Logger logger = LoggerFactory.getLogger(TemplateObjectBundleManager.class);
    protected boolean correctlyParsed = false;

    public TemplateObjectBundleManager(
            TemplateDTO templateDTO, Map<String, Archetype> archetypeMap,
            TerminologyService terminologyService,
            UserConfigurationManager userConfigurationManager) {
        this.templateDTO = templateDTO;
        this.archetypeMap = archetypeMap;
        this.terminologyService = terminologyService;
        this.userConfigurationManager = userConfigurationManager;
    }

    public void buildArchetypeObjectBundleCustomVO() {
        Object obj = null;
        if (templateDTO.getAobcVO() != null) {
            obj = SerializationUtils.deserialize(templateDTO.getAobcVO());
        }
        if (!(obj instanceof ArchetypeObjectBundleCustomVO)) {
            long startTime = System.currentTimeMillis();
            try {
                generateTemplateData();
                correctlyParsed = true;
            } catch (Error | Exception ex) {
                throw new RuntimeException(format("Failed to parsing template '%s'", templateDTO.getId()), ex);
            }
            long endTime = System.currentTimeMillis();
            logger.info(format("Template '%s' parsed successfully (%s ms)", templateDTO.getId(), (endTime - startTime)));
        } else {
            correctlyParsed = true;
        }
    }

    private void generateTemplateData() throws Exception {
        TEMPLATE template = getParsedTemplate(templateDTO.getSource());
        templateDTO.setArchetypeId(template.getDefinition().getArchetypeId());
        Archetype ar = new TemplateFlattener().toFlattenedArchetype(template, archetypeMap);
        templateDTO.setAom(SerializationUtils.serialize(ar));
        GenericObjectBundleADLManager genericObjectBundleADLManager =
                new GenericObjectBundleADLManager(ar, templateDTO.getId(), archetypeMap, terminologyService, userConfigurationManager);
        ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = genericObjectBundleADLManager.generateObjectBundleCustomVO();
        templateDTO.setAobcVO(SerializationUtils.serialize(archetypeObjectBundleCustomVO));
    }

    public static TEMPLATE getParsedTemplate(String templateSrc) throws InternalErrorException {
        try {
            OETParser parser = new OETParser();
            InputStream is = IOUtils.toInputStream(templateSrc, "UTF-8");
            return parser.parseTemplate(is).getTemplate();
        } catch (Exception ex) {
            throw new InternalErrorException(ex);
        }
    }
}
