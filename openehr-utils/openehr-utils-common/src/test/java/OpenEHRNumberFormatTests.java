import junit.framework.TestCase;
import se.cambio.openehr.util.OpenEHRNumberFormat;

import java.text.DecimalFormat;

/**
 * User: Iago.Corbal
 * Date: 2014-05-26
 * Time: 16:53
 */
public class OpenEHRNumberFormatTests extends TestCase {

    public void testRoundToStr(){
        assertEquals("20.1234", OpenEHRNumberFormat.roundToStr(new Double(20.1234), 4));
        assertEquals("2012340000000.0000", OpenEHRNumberFormat.roundToStr(2.01234E12, 4));
        assertEquals("2012340000000", OpenEHRNumberFormat.roundToStr(2.01234E12, 0));
        assertEquals("0.0002", OpenEHRNumberFormat.roundToStr(2.01234E-4, 4));
        assertEquals("1.0000", OpenEHRNumberFormat.roundToStr(1, 4));
    }

    public void testGetDecimalFormat(){
        DecimalFormat df = OpenEHRNumberFormat.getDecimalFormat();
        assertEquals("12.345", df.format(12.345));

        df = OpenEHRNumberFormat.getDecimalFormat(5);
        assertEquals("12.34500", df.format(12.345));
        assertEquals("12345.00000", df.format(1.2345E4));
        assertEquals("0.00012", df.format(1.2345E-4));
    }
}
