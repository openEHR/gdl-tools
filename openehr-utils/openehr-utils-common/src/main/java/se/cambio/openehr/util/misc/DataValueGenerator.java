package se.cambio.openehr.util.misc;

import com.rits.cloning.Cloner;
import org.joda.time.DateTime;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.basic.DvBoolean;
import org.openehr.rm.datatypes.encapsulated.DvParsable;
import org.openehr.rm.datatypes.quantity.*;
import org.openehr.rm.datatypes.quantity.datetime.DvDate;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDuration;
import org.openehr.rm.datatypes.quantity.datetime.DvTime;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import static se.cambio.openehr.util.OpenEHRDataValues.*;


public class DataValueGenerator {

    private static final Map<String, DataValue> dataValueMap;

    private static Logger logger = LoggerFactory.getLogger(DataValueGenerator.class);

    static {
        dataValueMap = new HashMap<>();
        dataValueMap.put(DV_COUNT, new DvCount(0));
        dataValueMap.put(DV_QUANTITY, new DvQuantity(10));
        dataValueMap.put(DV_TEXT, new DvText("text"));
        dataValueMap.put(DV_CODED_TEXT, new DvCodedText("text", new CodePhrase("tm", "cd")));
        dataValueMap.put(DV_ORDINAL, new DvOrdinal(0, new DvCodedText("text", new CodePhrase("tm", "cd"))));
        dataValueMap.put(DV_DATE_TIME, new DvDateTime());
        dataValueMap.put(DV_DATE, new DvDate());
        dataValueMap.put(DV_TIME, new DvTime());
        dataValueMap.put(DV_DURATION, new DvDuration("P10D"));
        dataValueMap.put(DV_BOOLEAN, new DvBoolean(Boolean.FALSE));
        dataValueMap.put(DV_PROPORTION, new DvProportion(1, 1, ProportionKind.UNITARY, 0));
        dataValueMap.put(DV_PARSABLE, new DvParsable("text", "txt"));
    }

    public static DataValue createDV(String rmName) {
        return new Cloner().deepClone(getDummyDV(rmName));
    }

    public static DataValue createDV(DataValue dataValue, String attributeName, Object value) throws InternalErrorException {
        if (dataValue instanceof DvQuantity) {
            return create((DvQuantity) dataValue, attributeName, value);
        } else if (dataValue instanceof DvDuration) {
            return create((DvDuration) dataValue, attributeName, value);
        } else if (dataValue instanceof DvDateTime) {
            return create((DvDateTime) dataValue, attributeName, value);
        } else if (dataValue instanceof DvDate) {
            return create((DvDate) dataValue, attributeName, value);
        } else if (dataValue instanceof DvTime) {
            return create((DvTime) dataValue, attributeName, value);
        } else if (dataValue instanceof DvOrdinal) {
            return create((DvOrdinal) dataValue, attributeName, value);
        } else if (dataValue instanceof DvCount) {
            return create((DvCount) dataValue, attributeName, value);
        } else if (dataValue instanceof DvCodedText) {
            return create((DvCodedText) dataValue, attributeName, value);
        } else if (dataValue instanceof DvText) {
            return create((DvText) dataValue, attributeName, value);
        } else if (dataValue instanceof DvBoolean) {
            return create((DvBoolean) dataValue, attributeName, value);
        } else if (dataValue instanceof DvProportion) {
            return create((DvProportion) dataValue, attributeName, value);
        } else {
            throw new IllegalArgumentException("Unknown data value '" + dataValue.getClass().getSimpleName() + "'");
        }
    }

    private static DvQuantity create(DvQuantity dvQuantity, String attributeName, Object value) throws InternalErrorException {

        Double magnitude = dvQuantity.getMagnitude();
        Integer precision = dvQuantity.getPrecision();
        String units = dvQuantity.getUnits();

        switch (attributeName) {
            case "magnitude":
                magnitude = Double.parseDouble(value.toString());
                break;
            case "units":
                if (value != null) {
                    units = value.toString();
                }
                break;
            case "precision":
                precision = (Integer) value;
                break;
            default:
        }
        return new DvQuantity(units, magnitude, precision);
    }

    private static DvDuration create(DvDuration dvDuration, String attributeName, Object value) throws InternalErrorException {

        String durationValue = dvDuration.getValue();

        if (attributeName.equals("value")) {
            durationValue = (String) value;
        }

        return new DvDuration(durationValue);
    }

    private static DvDateTime create(DvDateTime dvDateTime, String attributeName, Object value) throws InternalErrorException {

        Calendar cal = Calendar.getInstance();
        if (value instanceof Integer) {
            setCalendar(cal, Calendar.YEAR, attributeName, (Integer) value, "year", dvDateTime.getYear());
            setCalendar(cal, Calendar.MONTH, attributeName,
                    //We need to subtract one because calendar month starts at 0
                    ((Integer) value - 1), "month", dvDateTime.getMonth() - 1);
            setCalendar(cal, Calendar.DATE, attributeName, (Integer) value, "day", dvDateTime.getDay());
            setCalendar(cal, Calendar.HOUR_OF_DAY, attributeName, (Integer) value, "hour", dvDateTime.getHour());
            setCalendar(cal, Calendar.MINUTE, attributeName, (Integer) value, "minute", dvDateTime.getMinute());
            setCalendar(cal, Calendar.SECOND, attributeName, (Integer) value, "second", dvDateTime.getSecond());
        } else {
            if ("value".equals(attributeName) && value instanceof Long) {
                cal.setTimeInMillis(((Long) value));
            } else if ("value".equals(attributeName) && value instanceof Double) {
                cal.setTimeInMillis(((Double) value).longValue());
            } else {
                logger.warn("Wrong attribute name or class creating DvDateTime with value = '" + value + "'");
            }
        }
        return toDvDateTime(cal);
    }

    private static DvDate create(DvDate dvDate, String attributeName, Object value) throws InternalErrorException {
        Calendar cal = Calendar.getInstance();
        setCalendar(cal, Calendar.YEAR, attributeName, (Integer) value, "year", dvDate.getYear());
        setCalendar(cal, Calendar.MONTH, attributeName, ((Integer) value) - 1, "month", dvDate.getMonth() - 1);     //We need to subtract one because calendar month starts at 0
        setCalendar(cal, Calendar.DATE, attributeName, (Integer) value, "day", dvDate.getDay());
        return new DvDate(
                cal.get(Calendar.YEAR),
                cal.get(Calendar.MONTH) + 1,
                cal.get(Calendar.DATE));
    }

    private static DvCount create(DvCount dvCount, String attributeName, Object value) {
        int magnitude = dvCount.getMagnitude();
        if (attributeName.equals("magnitude")) {
            if (value instanceof Integer) {
                magnitude = ((Integer) value);
            } else if (value instanceof Double) {
                magnitude = ((Double) value).intValue();
            } else {
                logger.warn("Unkown class for count: " + value.getClass().getName());
            }
        }
        return new DvCount(magnitude);
    }

    private static DvCodedText create(DvCodedText dvCodedText, String attributeName, Object value) throws InternalErrorException {
        String codedTextvalue = dvCodedText.getValue();
        String terminologyId = dvCodedText.getDefiningCode().getTerminologyId().getValue();
        String code = dvCodedText.getDefiningCode().getCodeString();
        switch (attributeName) {
            case "value":
                codedTextvalue = (String) value;
                break;
            case "terminologyId":
                terminologyId = (String) value;
                break;
            case "code":
                code = (String) value;
                break;
            default:
        }
        return new DvCodedText(codedTextvalue, terminologyId, code);
    }

    private static DvText create(DvText dvText, String attributeName, Object value) throws InternalErrorException {
        String textValue = dvText.getValue();
        if (attributeName.equals("value")) {
            if (value != null) {
                textValue = "" + value;
            } else {
                textValue = null;
            }
        }
        return new DvText(textValue);
    }

    private static DvOrdinal create(DvOrdinal dvOrdinal, String attributeName, Object value) throws InternalErrorException {
        Integer ordinalValue = dvOrdinal.getValue();
        String codedTextvalue = dvOrdinal.getSymbolValue();
        String terminologyId = dvOrdinal.getTerminologyId();
        String code = dvOrdinal.getCode();
        switch (attributeName) {
            case "value":
                ordinalValue = (Integer) value;
                break;
            case "symbolValue":
                codedTextvalue = (String) value;
                break;
            case "terminologyId":
                terminologyId = (String) value;
                break;
            case "code":
                code = (String) value;
                break;
            default:
        }
        return new DvOrdinal(ordinalValue, codedTextvalue, terminologyId, code);
    }

    private static DvProportion create(DvProportion dvProportion, String attributeName, Object value) throws InternalErrorException {
        Double numerator = dvProportion.getNumerator();
        Double denominator = dvProportion.getDenominator();
        Integer precision = dvProportion.getPrecision();
        ProportionKind type = dvProportion.getType();
        switch (attributeName) {
            case "numerator":
                numerator = (Double) value;
                break;
            case "denominator":
                denominator = (Double) value;
                break;
            case "precision":
                precision = (Integer) value;
                break;
            case "type":
                type = (ProportionKind) value;
                break;
            default:
        }
        return new DvProportion(numerator, denominator, type, precision);
    }

    private static DvTime create(DvTime dvTime, String attributeName, Object value) throws InternalErrorException {
        Calendar cal = Calendar.getInstance();
        setCalendar(cal, Calendar.HOUR_OF_DAY, attributeName, (Integer) value, "hour", dvTime.getHour());
        setCalendar(cal, Calendar.MINUTE, attributeName, (Integer) value, "minute", dvTime.getMinute());
        setCalendar(cal, Calendar.SECOND, attributeName, (Integer) value, "second", dvTime.getSecond());
        TimeZone timeZone = Calendar.getInstance().getTimeZone();
        return new DvTime(
                cal.get(Calendar.HOUR_OF_DAY),
                cal.get(Calendar.MINUTE),
                cal.get(Calendar.SECOND), timeZone);
    }

    private static DvBoolean create(DvBoolean dvBoolean, String attributeName, Object value) throws InternalErrorException {
        Boolean booleanValue = dvBoolean.getValue();
        if (attributeName.equals("value")) {
            booleanValue = (Boolean) value;
        }
        return new DvBoolean(booleanValue);
    }

    private static void setCalendar(Calendar cal, int field, String attributeName, Integer value, String currentAttributeName, int fieldClonedValue) {
        cal.set(field, fieldClonedValue);
        if (currentAttributeName.equals(attributeName)) {
            cal.set(field, value);
        }
    }

    public static DvDateTime toDvDateTime(Calendar cal) {
        return new DvDateTime(new DateTime(cal.getTimeInMillis()).toString());
    }

    public static DataValue getDummyDV(String rmName) {
        DataValue dv = dataValueMap.get(rmName);
        if (dv == null) {
            throw new IllegalArgumentException("unsupported RM class[" + rmName + "]");
        } else {
            return dv;
        }
    }

}
