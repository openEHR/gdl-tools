package se.cambio.cm.configuration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Profile;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.generic.dao.GenericCMElementDAO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.configuration.UserConfiguration;

@Configuration
@Profile("cm-admin-file-dao")
@Import({UserConfiguration.class})
public class FileClinicalModelsPersistenceConfiguration {

    @Autowired
    UserConfigurationManager userConfigurationManager;

    @Bean(name = "ArchetypeDAO")
    public GenericCMElementDAO<ArchetypeDTO> archetypeDAO() {
        return new FileGenericCMElementDAO<>(ArchetypeDTO.class, userConfigurationManager.getArchetypeFolder());
    }

    @Bean(name = "TemplateDAO")
    public GenericCMElementDAO<TemplateDTO> templateDTO() {
        return new FileGenericCMElementDAO<>(TemplateDTO.class, userConfigurationManager.getTemplateFolder());
    }

    @Bean(name = "TerminologyDAO")
    public GenericCMElementDAO<TerminologyDTO> terminologyDAO() {
        return new FileGenericCMElementDAO<>(TerminologyDTO.class, userConfigurationManager.getTerminologiesFolder());
    }

    @Bean(name = "GuideDAO")
    public GenericCMElementDAO<GuideDTO> guideDAO() {
        return new FileGenericCMElementDAO<>(GuideDTO.class, userConfigurationManager.getGuidesFolder());
    }
}
