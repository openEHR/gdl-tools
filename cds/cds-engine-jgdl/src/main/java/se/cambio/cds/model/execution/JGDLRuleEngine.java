package se.cambio.cds.model.execution;

import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.parser.GDLParser;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.ExecutionLog;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.jgdl.DataInstance;
import se.cambio.jgdl.Interpreter;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Component
@Profile("rule-jgdl-engine")
public class JGDLRuleEngine implements RuleEngineFacadeDelegate {
    private static Logger LOGGER = LoggerFactory.getLogger(JGDLRuleEngine.class);
    private Map<String, Guide> guideCache = new ConcurrentHashMap<>();
    private boolean useCache = true;
    private GDLParser parser = new GDLParser();
    private Interpreter interpreter = new Interpreter();

    @Override
    public RuleExecutionResult execute(String ehrId, List<GuideDTO> guides,
                                       Collection<ArchetypeReference> archetypeReferences, Calendar date) {
        DvDateTime dateTime = new DvDateTime(
                date.get(Calendar.YEAR),
                date.get(Calendar.MONTH) + 1,
                date.get(Calendar.DAY_OF_MONTH),
                date.get(Calendar.HOUR),
                date.get(Calendar.MINUTE),
                date.get(Calendar.SECOND),
                date.get(Calendar.MILLISECOND) / 1000,
                date.getTimeZone()
        );
        interpreter.setSystemParameter("currentDateTime", dateTime);

        List<Guide> compiledGuides = new ArrayList<>();
        try {
            for (GuideDTO guideDTO : guides) {
                Guide guide = guideCache.get(guideDTO.getId());
                if (guide == null || !useCache) {
                    InputStream inputStream = new ByteArrayInputStream(guideDTO.getSource().getBytes(StandardCharsets.UTF_8));
                    guide = parser.parse(inputStream);
                    guideCache.put(guideDTO.getId(), guide);
                }
                compiledGuides.add(guide);
            }
        } catch (Exception e) {
            LOGGER.error("Error during execution ", e);
        }

        List<DataInstance> dataInstances = toDataInstanceList(archetypeReferences);
        Set<String> cdsDomainArchetypeIds = toCDSDomainArchetypeIds(archetypeReferences);
        List<DataInstance> dataInstanceResults = interpreter.executeGuides(compiledGuides, dataInstances);
        return new RuleExecutionResult(
                ehrId, date.getTime(),
                toCDSDomainArchetypeReferenceList(dataInstanceResults, cdsDomainArchetypeIds),
                new ArrayList<ExecutionLog>(),
                new ArrayList<RuleReference>());
    }

    private Set<String> toCDSDomainArchetypeIds(Collection<ArchetypeReference> archetypeReferences) {
        Set<String> refs = new HashSet<>();
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            if ("CDS".equals(archetypeReference.getIdDomain())) {
                refs.add(archetypeReference.getIdArchetype());
            }
        }
        return refs;
    }

    private String removeId(String path) {
        return path.substring(path.indexOf('/'), path.length());
    }

    private List<DataInstance> toDataInstanceList(Collection<ArchetypeReference> archetypeReferences) {
        List<DataInstance> ret = new ArrayList<>();
        for (ArchetypeReference archetypeReference : archetypeReferences) {
            if (!archetypeReference.getIdDomain().equals("CDS")) {
                DataInstance instance = new DataInstance.Builder()
                        .archetypeId(archetypeReference.getIdArchetype())
                        .build();
                for (Map.Entry<String, ElementInstance> entry : archetypeReference.getElementInstancesMap().entrySet()) {
                    if (entry.getValue().getDataValue() != null) {
                        instance.setValue(removeId(entry.getKey()), entry.getValue().getDataValue());
                    }
                }
                if (!instance.values().isEmpty()) {
                    ret.add(instance);
                }
            }
        }
        return ret;
    }

    private List<ArchetypeReference> toCDSDomainArchetypeReferenceList(List<DataInstance> dataInstance, Set<String> cdsDomainArchetypes) {
        List<ArchetypeReference> ret = new ArrayList<>();
        for (DataInstance instance : dataInstance) {
            if (cdsDomainArchetypes.contains(instance.archetypeId())) {
                ArchetypeReference archetypeReference = new ArchetypeReference("CDS", instance.archetypeId(), null);
                for (Map.Entry<String, DataValue> entry : instance.values().entrySet()) {
                    new ElementInstance(archetypeReference.getIdArchetype() + entry.getKey(), entry.getValue(), archetypeReference, null, null);
                }
                if (!archetypeReference.getElementInstancesMap().isEmpty()) {
                    ret.add(archetypeReference);
                }
            }
        }
        return ret;
    }

    @Override
    public void cancelExecution() {
    }

    @Override
    public void clearCache() {
        guideCache.clear();
    }

    @Override
    public void setUseCache(boolean useCache) {
        this.useCache = useCache;
    }

    @Override
    public byte[] compile(Guide guide) {
        return new byte[0];
    }
}
