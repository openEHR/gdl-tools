package se.cambio.cds.gdl.converters.drools;

import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.GuideCompiler;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * User: iago.corbal
 * Date: 2014-05-09
 * Time: 11:28
 */
public class DroolsGuideCompiler implements GuideCompiler{

    public byte[] compile(Guide guide) throws InternalErrorException {
        try {
            String droolsGuide = new GDLDroolsConverter(guide, ArchetypeManager.getInstance()).convertToDrools();
            return CompilationManager.compile(droolsGuide);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }
}
