package se.cambio.cm.model.util;

import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class CMTypeManager {

    private static CMTypeManager instance;
    private Map<String, CMType> cmTypeByIdMap;

    private CMTypeManager(){
        registerCMType(new CMType("terminologies", TerminologyDTO.class, Collections.singleton(CMTypeFormat.CSV_FORMAT.getFormat())));
        registerCMType(new CMType("archetypes", ArchetypeDTO.class, Arrays.asList(CMTypeFormat.ADL_FORMAT.getFormat(), CMTypeFormat.ADLS_FORMAT.getFormat())));
        registerCMType(new CMType("templates", TemplateDTO.class, Collections.singleton(CMTypeFormat.OET_FORMAT.getFormat())));
        registerCMType(new CMType("guidelines", GuideDTO.class, Collections.singleton(CMTypeFormat.GDL_FORMAT.getFormat())));
        registerAdditionalCMTypes();
    }

    private void registerAdditionalCMTypes() {
        try {
            for (CMType cmType: CMConfigurationManager.getAdditionalCMElements()){
                registerCMType(cmType);
            }
        } catch (MissingConfigurationParameterException e) {
            ExceptionHandler.handle(e);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
    }

    private void registerCMType(CMType cmType) {
        getCmTypeByIdMap().put(cmType.getId(), cmType);
    }

    public CMType getCMTypeById(String id) throws InstanceNotFoundException {
        CMType cmType = getCmTypeByIdMap().get(id);
        if (cmType != null) {
            return cmType;
        } else {
            throw new InstanceNotFoundException(id, CMTypeManager.class.getName());
        }
    }

    public CMType getCMTypeByClass(Class<? extends CMElement> cmElementClass) throws InternalErrorException {
        for (CMType cmType : getAllCMTypes()){
            if (cmType.getCmElementClass().equals(cmElementClass)){
                return cmType;
            }
        }
        throw new InternalErrorException(new InstanceNotFoundException(cmElementClass.getName(), CMElement.class.getName()));
    }

    public Class<? extends CMElement> getCMElementClassById(String id) throws InstanceNotFoundException {
        CMType cmType = getCMTypeById(id);
        return cmType.getCmElementClass();
    }

    public Collection<CMType> getAllCMTypes(){
        return getCmTypeByIdMap().values();
    }


    private Map<String, CMType> getCmTypeByIdMap() {
        if (cmTypeByIdMap == null) {
            cmTypeByIdMap = new LinkedHashMap<String, CMType>();
        }
        return cmTypeByIdMap;
    }

    public static CMTypeManager getInstance() {
        if (instance == null) {
            instance = new CMTypeManager();
        }
        return instance;
    }
}
