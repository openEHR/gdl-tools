package se.cambio.cds.gdl.converters.drools;

import org.apache.log4j.Logger;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.ByteArrayInputStream;

/**
 * User: iago.corbal
 * Date: 2014-05-09
 * Time: 11:28
 */
public class DroolsGuideUtil {

    public static void compileIfNeeded(GuideDTO guideDTO) throws InternalErrorException {
        compile(guideDTO, false);
    }

    public static void compile(GuideDTO guideDTO, boolean force) throws InternalErrorException {
        try {
            long startTime = System.currentTimeMillis();
            boolean compile = false;
            Guide guide = null;
            if (guideDTO.getGuideObject()==null || force){
                guide = GuideUtil.parseGuide(new ByteArrayInputStream(guideDTO.getGuideSrc().getBytes()));
                guideDTO.setGuideObject(IOUtils.getBytes(guide));
            }else{
                guide = (Guide)IOUtils.getObject(guideDTO.getGuideObject());
            }
            if (guideDTO.getCompiledGuide()==null || force){
                String droolsGuide = new GDLDroolsConverter(guide).convertToDrools();
                guideDTO.setCompiledGuide(CompilationManager.compile(droolsGuide));
                compile = true;
            }
            if (compile){
                long totalExecutionTime = System.currentTimeMillis()-startTime;
                Logger.getLogger(DroolsGuideUtil.class).info("Guide '"+guideDTO.getIdGuide()+"' compiled in "+totalExecutionTime+" ms.");
            }
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }
}
