import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;

/**
 * User: Iago.Corbal
 * Date: 2014-02-27
 * Time: 15:42
 */
public class CheckResources {

    public static void main(String[] args){
        try {
            Collection<ArchetypeDTO> archetypeDTO = OpenEHRSessionManager.getAdministrationFacadeDelegate().searchAllArchetypes();
            System.out.println("num = "+archetypeDTO.size());
            Collection<TemplateDTO> templateDTOs = OpenEHRSessionManager.getAdministrationFacadeDelegate().searchAllTemplates();
            System.out.println("num = "+templateDTOs.size());
            Collection<TerminologyDTO> terminologiesDTOs = OpenEHRSessionManager.getAdministrationFacadeDelegate().searchAllTerminologies();
            System.out.println("num = "+terminologiesDTOs.size());
        } catch (InternalErrorException e) {
            e.printStackTrace();
        }
    }
}
