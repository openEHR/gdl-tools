package se.cambio.cds.util;

import org.junit.Test;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.DVUtil;

import java.util.HashMap;
import java.util.Map;

import static junit.framework.TestCase.assertEquals;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class DvUtilTest {
    @Test
    public void shouldCacheTerminologyResolution() {
        ElementInstance ei = new ElementInstance("testId1", new DvCodedText("test0", "ICD10", "I489"), null, null, null);
        Map<ElementInstance, Map<String, Boolean>> bindingsMap = new HashMap<>();
        DataValue dv1 = new DvCodedText("test1", "ICD10", "I48");
        DataValue dv2 = new DvCodedText("test2", "ICD10", "B1");
        DataValue dv3 = new DvCodedText("test3", "ICD10", "C1");
        boolean subClassOf = DVUtil.isSubClassOf(false, ei, bindingsMap, "gt0001", dv1, dv2, dv3);
        assertTrue(subClassOf);
        Map<String, Boolean> map = bindingsMap.get(ei);
        assertEquals(1, map.size());
        DataValue dvAux1 = new DvCodedText("test1", "ICD10", "I48");
        DataValue dvAux2 = new DvCodedText("test2", "ICD10", "B1");
        DataValue dvAux3 = new DvCodedText("test3", "ICD10", "C1");
        subClassOf = DVUtil.isSubClassOf(false, ei, bindingsMap, "gt0001", dvAux1, dvAux2, dvAux3);
        assertTrue(subClassOf);
        map = bindingsMap.get(ei);
        assertEquals(1, map.size());
    }

    @Test
    public void shouldCalculateDurationAgainstDateTime() {
        Double aDouble = DVUtil.calculateDurationAgainstDvTemporal("365, d", new DvDateTime("2016-01-01T12:00:00"), "-");
        assertThat(aDouble, equalTo(3.1536E10));
    }

    @Test
    public void shouldCalculateDurationAgainstDateTimeWithDouble() {
        Double aDouble = DVUtil.calculateDurationAgainstDvTemporal("365.0,d", new DvDateTime("2016-01-01T12:00:00"), "-");
        assertThat(aDouble, equalTo(3.1536E10));
    }

    @Test
    public void shouldCalculateDurationAgainstStringDateTime() {
        Double aDouble = DVUtil.calculateDuration("1,d", "2016-01-01T12:00:00", "-");
        Double bDouble = DVUtil.calculateDurationAgainstDvTemporal("1, d", new DvDateTime("2016-01-01T12:00:00"), "-");
        assertThat(aDouble, equalTo(bDouble));
    }

    @Test
    public void shouldCalculateDurationAgainstDoubleValue() {
        Double aDouble = DVUtil.calculateDuration("1,d", 1, "-");
        assertThat(aDouble, equalTo(8.64E7));
    }
}
