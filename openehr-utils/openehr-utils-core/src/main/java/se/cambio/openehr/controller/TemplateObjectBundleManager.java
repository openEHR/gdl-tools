package se.cambio.openehr.controller;

import openEHR.v1.template.TEMPLATE;
import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.template.FlatteningException;
import org.openehr.am.template.OETParser;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.TemplateFlattener;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.InputStream;
import java.util.Map;

public class TemplateObjectBundleManager {

    private TemplateDTO templateDTO = null;
    private final Map<String, Archetype> archetypeMap;
    protected boolean correctlyParsed = false;

    public TemplateObjectBundleManager(TemplateDTO templateDTO, Map<String, Archetype> archetypeMap) {
        this.templateDTO = templateDTO;
        this.archetypeMap = archetypeMap;
    }

    public void buildArchetypeObjectBundleCustomVO() throws InternalErrorException {
        Object obj = null;
        if (templateDTO.getAobcVO() != null){
            obj = IOUtils.getObject(templateDTO.getAobcVO());
        }
        if (!(obj instanceof ArchetypeObjectBundleCustomVO)){
            Logger.getLogger(TemplateObjectBundleManager.class).info("Parsing template '"+templateDTO.getId()+"'...");
            long startTime = System.currentTimeMillis();
            try{
                generateTemplateData();
                correctlyParsed = true;
            }catch(InternalErrorException e){
                throw e;
            }catch(Error e){
                new InternalErrorException(new Exception("Failed to parse template '"+templateDTO.getId()+"'", e));
            }catch(Exception e){
                new InternalErrorException(new Exception("Failed to parse template '"+templateDTO.getId()+"'", e));
            }
            long endTime = System.currentTimeMillis();
            Logger.getLogger(TemplateObjectBundleManager.class).info("Done (" + (endTime - startTime) + " ms)");
        }else{
            correctlyParsed = true;
        }
    }

    private void generateTemplateData()
            throws InternalErrorException {
        try {
            TEMPLATE template = getParsedTemplate(templateDTO.getSource());
            templateDTO.setArcehtypeId(template.getDefinition().getArchetypeId());
            Archetype ar = new TemplateFlattener().toFlattenedArchetype(template, archetypeMap);
            templateDTO.setAom(IOUtils.getBytes(ar));
            GenericObjectBundleADLManager genericObjectBundleADLManager = new GenericObjectBundleADLManager(ar, templateDTO.getId(), archetypeMap);
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = genericObjectBundleADLManager.generateObjectBundleCustomVO();
            templateDTO.setAobcVO(IOUtils.getBytes(archetypeObjectBundleCustomVO));
        } catch (FlatteningException e) {
            throw new InternalErrorException(e);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    public static TEMPLATE getParsedTemplate(String templateSrc) throws InternalErrorException {
        try {
            OETParser parser = new OETParser();
            InputStream is = IOUtils.toInputStream(templateSrc, "UTF-8");
            return parser.parseTemplate(is).getTemplate();
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }
}
