import org.apache.commons.lang.StringUtils;
import org.junit.Test;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.basic.DvBoolean;
import org.openehr.rm.datatypes.quantity.*;
import org.openehr.rm.datatypes.quantity.datetime.DvDate;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDuration;
import org.openehr.rm.datatypes.quantity.datetime.DvTime;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.misc.DataValueGenerator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class DataValueGeneratorTest {
    @Test
    public void shouldModifyDvQuantity() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvQuantity("mg", 2.0, 1), "magnitude", 3);
        dv = DataValueGenerator.createDV(dv, "units", "kg");
        dv = DataValueGenerator.createDV(dv, "precision", 2);
        assertTrue(dv instanceof DvQuantity);
        assertEquals(((DvQuantity) dv).getMagnitude(), 3, 0);
        assertEquals(((DvQuantity)dv).getUnits(), "kg");
        assertEquals(((DvQuantity)dv).getPrecision(), 2, 0);
    }

    @Test
    public void shouldModifyDvText() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvText("test"), "value", "test1");
        assertTrue(dv instanceof DvText);
        assertEquals(((DvText) dv).getValue(), "test1");
    }

    @Test
    public void shouldModifyDvCount() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvCount(1), "magnitude", 2);
        assertTrue(dv instanceof DvCount);
        assertEquals((long)((DvCount) dv).getMagnitude(), 2);
    }

    @Test
    public void shouldModifyDvDateTime() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvDateTime(), "year", 2015);
        dv = DataValueGenerator.createDV(dv, "month", 1);
        dv = DataValueGenerator.createDV(dv, "day", 2);
        dv = DataValueGenerator.createDV(dv, "hour", 12);
        dv = DataValueGenerator.createDV(dv, "minute", 59);
        dv = DataValueGenerator.createDV(dv, "second", 11);
        assertTrue(dv instanceof DvDateTime);
        String value = StringUtils.substringBeforeLast(((DvDateTime) dv).getValue(), ".");
        assertEquals(value, "2015-01-02T12:59:11");
        assertEquals((long)((DvDateTime) dv).getYear(), 2015);
        assertEquals((long)((DvDateTime) dv).getMonth(), 1);
        assertEquals((long)((DvDateTime) dv).getDay(), 2);
        assertEquals((long)((DvDateTime) dv).getHour(), 12);
        assertEquals((long)((DvDateTime) dv).getMinute(), 59);
        assertEquals((long)((DvDateTime) dv).getSecond(), 11);
    }

    @Test
    public void shouldModifyDvDuration() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvDuration("P10D"), "value", "P1Y");
        assertTrue(dv instanceof DvDuration);
        assertEquals(((DvDuration) dv).getValue(), "P1Y");
    }

    @Test
    public void shouldModifyDvDate() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvDate(), "year", 2015);
        dv = DataValueGenerator.createDV(dv, "month", 1);
        dv = DataValueGenerator.createDV(dv, "day", 2);
        assertTrue(dv instanceof DvDate);
        assertEquals(((DvDate) dv).getValue(), "2015-01-02");
        assertEquals((long)((DvDate) dv).getYear(), 2015);
        assertEquals((long)((DvDate) dv).getMonth(), 1);
        assertEquals((long)((DvDate) dv).getDay(), 2);
    }

    @Test
    public void shouldModifyDvTime() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvTime(), "hour", 12);
        dv = DataValueGenerator.createDV(dv, "minute", 59);
        dv = DataValueGenerator.createDV(dv, "second", 11);
        assertTrue(dv instanceof DvTime);
        String value = StringUtils.substringBeforeLast(((DvTime) dv).getValue(), ".");
        assertEquals(((DvTime) dv).getValue(), value);
        assertEquals((long)((DvTime) dv).getHour(), 12);
        assertEquals((long)((DvTime) dv).getMinute(), 59);
        assertEquals((long)((DvTime) dv).getSecond(), 11);
    }

    @Test
         public void shouldModifyDvCodedText() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvCodedText("testDesc", new CodePhrase("testTermId", "testCode")), "value", "testDesc1");
        dv = DataValueGenerator.createDV(dv, "terminologyId", "testTermId1");
        dv = DataValueGenerator.createDV(dv, "code", "testCode1");
        assertTrue(dv instanceof DvCodedText);
        assertEquals(((DvCodedText) dv).getValue(), "testDesc1");
        assertEquals(((DvCodedText) dv).getTerminologyId(), "testTermId1");
        assertEquals(((DvCodedText) dv).getCode(), "testCode1");
    }

    @Test
    public void shouldModifyDvOrdinal() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvOrdinal(1, "testDesc", "testTermId", "testCode"), "value", 2);
        dv = DataValueGenerator.createDV(dv, "symbolValue", "testDesc1");
        dv = DataValueGenerator.createDV(dv, "terminologyId", "testTermId1");
        dv = DataValueGenerator.createDV(dv, "code", "testCode1");
        assertTrue(dv instanceof DvOrdinal);
        assertEquals(((DvOrdinal) dv).getValue(), 2);
        assertEquals(((DvOrdinal) dv).getSymbolValue(), "testDesc1");
        assertEquals(((DvOrdinal) dv).getTerminologyId(), "testTermId1");
        assertEquals(((DvOrdinal) dv).getCode(), "testCode1");
    }

    @Test
    public void shouldModifyDvBoolean() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvBoolean(false), "value", true);
        assertTrue(dv instanceof DvBoolean);
        assertEquals(((DvBoolean) dv).getValue(), true);
    }

    @Test
    public void shouldModifyDvProportion() throws InternalErrorException {
        DataValue dv = DataValueGenerator.createDV(new DvProportion(1.0, 2.0, ProportionKind.FRACTION, 0), "numerator", 3.0);
        dv = DataValueGenerator.createDV(dv, "type", ProportionKind.RATIO);
        dv = DataValueGenerator.createDV(dv, "precision", 0);
        dv = DataValueGenerator.createDV(dv, "denominator", 4.0);
        assertTrue(dv instanceof DvProportion);
        assertEquals(((DvProportion) dv).getNumerator(), 3.0, 0);
        assertEquals(((DvProportion) dv).getDenominator(), 4.0, 0);
        assertEquals(((DvProportion) dv).getType(), ProportionKind.RATIO);
        assertEquals((long)((DvProportion) dv).getPrecision(), 0);
    }
}
