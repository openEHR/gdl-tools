package se.cambio.openehr.controller;

import org.apache.commons.lang.SerializationUtils;
import org.openehr.adl.flattener.ArchetypeFlattener;
import org.openehr.adl.parser.AdlDeserializer;
import org.openehr.adl.rm.OpenEhrRmModel;
import org.openehr.am.archetype.Archetype;
import org.openehr.jaxb.am.DifferentialArchetype;
import org.openehr.jaxb.am.FlatArchetype;
import org.openehr.jaxb.rm.ArchetypeId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.acode.openehr.parser.ADLParser;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.cm.model.util.CMTypeFormat;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import static java.lang.String.format;

public class ArchetypeObjectBundleManager {
    private final ArchetypeManager archetypeManager;
    private ArchetypeDTO archetypeDTO = null;
    private Logger logger = LoggerFactory.getLogger(ArchetypeObjectBundleManager.class);

    public ArchetypeObjectBundleManager(ArchetypeDTO archetypeDTO,
                                        ArchetypeManager archetypeManager) {
        this.archetypeDTO = archetypeDTO;
        this.archetypeManager = archetypeManager;
    }

    public void buildArchetypeObjectBundleCustomVO() throws InternalErrorException {
        Object obj = null;
        if (archetypeDTO.getAobcVO() != null) {
            obj = SerializationUtils.deserialize(archetypeDTO.getAobcVO());
        }
        if (!(obj instanceof ArchetypeObjectBundleCustomVO)) {
            long startTime = System.currentTimeMillis();
            try {
                if (CMTypeFormat.ADL_FORMAT.getFormat().equals(archetypeDTO.getFormat())) {
                    generateArchetype14Data();
                } else if (CMTypeFormat.ADLS_FORMAT.getFormat().equals(archetypeDTO.getFormat())) {
                    generateArchetype20Data();
                }
            } catch (Error | Exception ex) {
                throw new RuntimeException(format("Failed to parsing archetype '%s'", archetypeDTO.getId()), ex);
            }
            long endTime = System.currentTimeMillis();
            logger.info(format("Archetype '%s' parsed successfully (%s ms)", archetypeDTO.getId(), (endTime - startTime)));
        }
    }


    private void generateArchetype14Data()
            throws InternalErrorException {
        try {
            ADLParser adlParser = new ADLParser(archetypeDTO.getSource());
            Archetype ar = adlParser.parse();
            archetypeDTO.setAom(SerializationUtils.serialize(ar));
            GenericObjectBundleADLManager genericObjectBundleADLManager =
                    new GenericObjectBundleADLManager(
                            ar, archetypeManager.getArchetypes().getArchetypeMap(),
                            archetypeManager.getTerminologyService(), archetypeManager.getUserConfigurationManager());
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = genericObjectBundleADLManager.generateObjectBundleCustomVO();
            archetypeDTO.setAobcVO(SerializationUtils.serialize(archetypeObjectBundleCustomVO));
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    private void generateArchetype20Data()
            throws InternalErrorException {
        try {
            AdlDeserializer adlDeserializer = new AdlDeserializer(new OpenEhrRmModel());
            DifferentialArchetype differentialArchetype = adlDeserializer.parse(archetypeDTO.getSource());
            FlatArchetype flatArchetype = parseAndFlattenArchetype(differentialArchetype);
            byte[] flatArchetypeBytes = SerializationUtils.serialize(flatArchetype);
            archetypeDTO.setAom(flatArchetypeBytes);
            GenericObjectBundleADLSManager genericObjectBundleADLSManager =
                    new GenericObjectBundleADLSManager(flatArchetype, archetypeManager);
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = genericObjectBundleADLSManager.generateObjectBundleCustomVO();
            archetypeDTO.setAobcVO(SerializationUtils.serialize(archetypeObjectBundleCustomVO));
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }

    private FlatArchetype parseAndFlattenArchetype(DifferentialArchetype differentialArchetype) throws InstanceNotFoundException, InternalErrorException {
        ArchetypeFlattener archetypeFlattener = new ArchetypeFlattener(new OpenEhrRmModel());
        FlatArchetype parent;
        ArchetypeId parentArchetypeId = differentialArchetype.getParentArchetypeId();
        if (parentArchetypeId != null) {
            parent = archetypeManager.getArchetypes().getArchetypeAOM2ById(parentArchetypeId.getValue());
        } else {
            parent = null;
        }
        return archetypeFlattener.flatten(parent, differentialArchetype);
    }
}
