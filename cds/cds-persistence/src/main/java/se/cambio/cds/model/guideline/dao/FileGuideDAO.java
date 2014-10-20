package se.cambio.cds.model.guideline.dao;

import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.openehr.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.util.UserConfigurationManager;

import java.util.Collections;

public class FileGuideDAO extends FileGenericCMElementDAO<GuideDTO> {

    public FileGuideDAO() {
        super(GuideDTO.class, UserConfigurationManager.getGuidesFolder(), Collections.singleton("gdl"));
    }
}
