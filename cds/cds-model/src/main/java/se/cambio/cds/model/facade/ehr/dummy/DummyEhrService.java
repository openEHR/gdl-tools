package se.cambio.cds.model.facade.ehr.dummy;

import org.joda.time.DateTime;
import se.cambio.cds.model.facade.ehr.delegate.EhrService;
import se.cambio.cds.model.facade.ehr.util.EHRDataStream;
import se.cambio.cds.model.instance.ArchetypeReference;

import java.util.*;

public class DummyEhrService implements EhrService {

    public DummyEhrService() {
    }

    @Override
    public List<List<Object>> query(String sql) {
        return null;
    }

    @Override
    public EHRDataStream queryStream(String sql) {
        return null;
    }

    @Override
    public Map<String, Collection<ArchetypeReference>> queryEHRElements(Collection<String> ehrIds, Collection<ArchetypeReference> archetypeReferences, Calendar date) {
        return null;
    }

    @Override
    public Set<String> fetchEhrIds(DateTime beforeTimestamp, DateTime afterTimestamp, Collection<String> archetypeIds) {
        return null;
    }
}
