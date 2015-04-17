import org.junit.Test;
import se.cambio.openehr.util.OpenEHRDataValues;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class DataValueTest {
    @Test
    public void should_be_detected_as_data_values() {
        assertTrue(OpenEHRDataValues.isDataValue("DV_CODED_TEXT"));
        assertTrue(OpenEHRDataValues.isDataValue("DV_ORDINAL"));
        assertTrue(OpenEHRDataValues.isDataValue("DV_COUNT"));
        assertTrue(OpenEHRDataValues.isDataValue("DV_URI"));
        assertTrue(OpenEHRDataValues.isDataValue("DV_DATE_TIME"));
        assertTrue(OpenEHRDataValues.isDataValue("DV_PARSABLE"));
        assertTrue(OpenEHRDataValues.isDataValue("DV_IDENTIFIER"));
        assertFalse(OpenEHRDataValues.isDataValue("CLUSTER"));
        assertFalse(OpenEHRDataValues.isDataValue("ELEMENT"));
        assertFalse(OpenEHRDataValues.isDataValue("value"));
    }
}
