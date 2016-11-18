package se.cambio.cm.model.guide.dao;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.UserConfigurationManager;

@Component("GuideDAO")
@Profile("cm-admin-file-dao")
public class FileGuideDAO extends FileGenericCMElementDAO<GuideDTO> {

    public FileGuideDAO() {
        super(GuideDTO.class, UserConfigurationManager.instance().getGuidesFolder());
    }
}
