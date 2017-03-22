package se.cambio.cm.model.facade.execute.dummy;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.Calendar;
import java.util.Collection;
import java.util.List;

@Component
@Profile("rule-dummy-engine")
public class DummyRuleEngineFacadeDelegate implements RuleEngineFacadeDelegate {
    @Override
    public RuleExecutionResult execute(String ehrId, List<GuideDTO> guides, Collection<ArchetypeReference> archetypeReferences, Calendar date) {
        return null;
    }

    @Override
    public void cancelExecution() {
    }

    @Override
    public void clearCache() {
    }

    @Override
    public void setUseCache(boolean useCache) {
    }

    @Override
    public byte[] compile(Guide guide) {
        return new byte[0];
    }
}
