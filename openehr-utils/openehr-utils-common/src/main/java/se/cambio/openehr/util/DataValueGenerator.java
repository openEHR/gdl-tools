package se.cambio.openehr.util;

import com.rits.cloning.Cloner;
import org.apache.log4j.Logger;
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

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

/**
 * User: iago.corbal
 * Date: 2013-11-29
 * Time: 08:52
 */
public class DataValueGenerator {

    private final static Map<String, DataValue> dataValueMap;

    /*
     * Initiate the mapping between ReferenceModelName and concrete dataValue
     */
    static {
        dataValueMap = new HashMap<String, DataValue>();
        dataValueMap.put(OpenEHRDataValues.DV_COUNT, new DvCount(0));
        dataValueMap.put(OpenEHRDataValues.DV_QUANTITY, new DvQuantity(10));
        dataValueMap.put(OpenEHRDataValues.DV_TEXT, new DvText("text"));
        dataValueMap.put(OpenEHRDataValues.DV_CODED_TEXT, new DvCodedText("text", new CodePhrase("tm", "cd")));
        dataValueMap.put(OpenEHRDataValues.DV_ORDINAL, new DvOrdinal(0, new DvCodedText("text", new CodePhrase("tm", "cd"))));
        dataValueMap.put(OpenEHRDataValues.DV_DATE_TIME, new DvDateTime("2001-02-11T00"));
        dataValueMap.put(OpenEHRDataValues.DV_DATE, new DvDate("2001-02-11"));
        dataValueMap.put(OpenEHRDataValues.DV_TIME, new DvTime("12:00:00"));
        dataValueMap.put(OpenEHRDataValues.DV_DURATION, new DvDuration("P10D"));
        dataValueMap.put(OpenEHRDataValues.DV_BOOLEAN, new DvBoolean(Boolean.FALSE));
        dataValueMap.put(OpenEHRDataValues.DV_PROPORTION, new DvProportion(1,1, ProportionKind.UNITARY,0));
    }

    public static DataValue createDV(String rmName){
        return new Cloner().deepClone(getDummyDV(rmName));
    }

    public static DataValue createDV(DataValue dataValue, String attributeName, Object value) throws InternalErrorException {
        if (dataValue instanceof DvQuantity){
            return create((DvQuantity)dataValue, attributeName, value);
        }if (dataValue instanceof DvDuration){
            return create((DvDuration)dataValue, attributeName, value);
        }if (dataValue instanceof DvDateTime){
            return create((DvDateTime)dataValue, attributeName, value);
        }if (dataValue instanceof DvDate){
            return create((DvDate)dataValue, attributeName, value);
        }if (dataValue instanceof DvTime){
            return create((DvTime)dataValue, attributeName, value);
        }if (dataValue instanceof DvOrdinal){
            return create((DvOrdinal)dataValue, attributeName, value);
        }if (dataValue instanceof DvCount){
            return create((DvCount)dataValue, attributeName, value);
        }if (dataValue instanceof DvCodedText){
            return create((DvCodedText)dataValue, attributeName, value);
        }if (dataValue instanceof DvText){
            return create((DvText)dataValue, attributeName, value);
        }if (dataValue instanceof DvBoolean){
            return create((DvBoolean)dataValue, attributeName, value);
        }else{
            throw new IllegalArgumentException("Unknown data value '"+dataValue.getClass().getSimpleName()+"'");
        }
    }

    private static DvQuantity create(DvQuantity dvQuantity, String attributeName, Object value) throws InternalErrorException{

        Double magnitude = dvQuantity.getMagnitude();
        Integer precision = dvQuantity.getPrecision();
        String units = dvQuantity.getUnits();

        if (attributeName.equals("magnitude")){
            magnitude = Double.parseDouble(value.toString());
        }else if (attributeName.equals("units")){
            if (value!=null){
                units = value.toString();
            }
        }else if (attributeName.equals("precision")){
            precision = (Integer)value;
        }
        return new DvQuantity(units, magnitude, precision);
    }

    private static DvDuration create(DvDuration dvDuration, String attributeName, Object value) throws InternalErrorException{

        String durationValue = dvDuration.getValue();

        if (attributeName.equals("value")){
            durationValue = (String)value;
        }

        return new DvDuration(durationValue);
    }

    private static DvDateTime create(DvDateTime dvDateTime, String attributeName, Object value) throws InternalErrorException{

        Calendar cal = Calendar.getInstance();
        if (value instanceof Integer){
            setCalendar(cal, Calendar.YEAR, attributeName, (Integer)value, "year", dvDateTime.getYear());
            setCalendar(cal, Calendar.MONTH, attributeName, (Integer)value, "month", dvDateTime.getMonth());
            setCalendar(cal, Calendar.DATE, attributeName, (Integer)value, "day", dvDateTime.getDay());
            setCalendar(cal, Calendar.HOUR, attributeName, (Integer)value, "hour", dvDateTime.getHour());
            setCalendar(cal, Calendar.MINUTE, attributeName, (Integer)value, "minute", dvDateTime.getMinute());
            setCalendar(cal, Calendar.SECOND, attributeName, (Integer)value, "second", dvDateTime.getSecond());
        }else{
            if ("value".equals(attributeName) && value instanceof Long){
                cal.setTimeInMillis(((Long)value));
            }else{
                Logger.getLogger(DataValueGenerator.class).warn("Wrong attribute name or class creating DvDateTime with value = '"+value+"'");
            }
        }
        return toDvDateTime(cal);
    }

    public static DvDateTime toDvDateTime(Calendar cal){
        //TODO "fractionalSecond";
        //TODO "timeZone";
        return new DvDateTime(
                cal.get(Calendar.YEAR),
                cal.get(Calendar.MONTH)+1,
                cal.get(Calendar.DATE),
                cal.get(Calendar.HOUR),
                cal.get(Calendar.MINUTE),
                cal.get(Calendar.SECOND), /*fractionalSecond,*/ TimeZone.getTimeZone("UTC"));
    }

    private static DvDate create(DvDate dvDate, String attributeName, Object value) throws InternalErrorException{
        Calendar cal = Calendar.getInstance();
        setCalendar(cal, Calendar.YEAR, attributeName, (Integer)value, "year", dvDate.getYear());
        setCalendar(cal, Calendar.MONTH, attributeName, (Integer)value, "month", dvDate.getMonth());
        setCalendar(cal, Calendar.DATE, attributeName, (Integer)value, "day", dvDate.getDay());
        return new DvDate(
                cal.get(Calendar.YEAR),
                cal.get(Calendar.MONTH)+1,
                cal.get(Calendar.DATE));
    }

    private static DvCount create(DvCount dvCount, String attributeName, Object value) throws InternalErrorException{
        int magnitude = dvCount.getMagnitude();
        if(attributeName.equals("magnitude")){
            if (value instanceof Integer){
                magnitude =((Integer)value);
            }else if (value instanceof Double){
                magnitude = ((Double)value).intValue();
            }else{
                Logger.getLogger(DataValueGenerator.class).warn("Unkown class for count: "+value.getClass().getName());
            }
        }
        return new DvCount(magnitude);
    }

    private static DvCodedText create(DvCodedText dvCodedText, String expressi, String attributeName, Object value) throws InternalErrorException{
        String codedTextvalue = dvCodedText.getValue();
        String terminologyId = dvCodedText.getDefiningCode().getTerminologyId().getValue();
        String code = dvCodedText.getDefiningCode().getCodeString();
        if (attributeName.equals("value")){
            codedTextvalue = (String)value;
        }else if (attributeName.equals("terminologyId")){
            terminologyId = (String)value;
        }else if (attributeName.equals("code")){
            code = (String) value;
        }
        return new DvCodedText(codedTextvalue, terminologyId, code);
    }

    private static DvText create(DvText dvText, String attributeName, Object value) throws InternalErrorException{
        String textValue = dvText.getValue();
        if (attributeName.equals("value")){
            textValue = (String)value;;
        }

        return new DvText(textValue);
    }

    private static DvOrdinal create(DvOrdinal dvOrdinal, String attributeName, Object value) throws InternalErrorException{
        Integer ordinalValue = dvOrdinal.getValue();
        if (attributeName.equals("value")){
            ordinalValue = (Integer)value;
        }
        return new DvOrdinal(ordinalValue, dvOrdinal.getSymbol());
    }

    private static DvTime create(DvTime dvTime, String attributeName, Object value) throws InternalErrorException{
        Calendar cal = Calendar.getInstance();
        setCalendar(cal, Calendar.HOUR, attributeName, (Integer)value, "hour", dvTime.getHour());
        setCalendar(cal, Calendar.MINUTE, attributeName, (Integer)value, "minute", dvTime.getMinute());
        setCalendar(cal, Calendar.SECOND, attributeName, (Integer)value, "second", dvTime.getSecond());
        //String[] assignation = aMap.get("timeZone");
        TimeZone timeZone =  Calendar.getInstance().getTimeZone();
        //TODO
        //if (assignation==null){
        //    timeZone = dvTime.getTimeZone();
        //}else{
        //  timeZone = assignation[2];
        //}
        return new DvTime(
                cal.get(Calendar.HOUR),
                cal.get(Calendar.MINUTE),
                cal.get(Calendar.SECOND), /*fractionalSecond,*/ timeZone);
    }

    private static DvBoolean create(DvBoolean dvBoolean, String attributeName, Object value) throws InternalErrorException{
        Boolean booleanValue = dvBoolean.getValue();
        if (attributeName.equals("value")){
            booleanValue = (Boolean) value;
        }
        return new DvBoolean(booleanValue);
    }

    private static void setCalendar(Calendar cal, int field, String attributeName, Integer value, String currentAttributeName, int fieldClonedValue){
        cal.set(field, fieldClonedValue);
        if (currentAttributeName.equals(attributeName)){
            cal.set(field, value);
        }
    }

    //SET LOG (TODO CHANGE)
    public static DataValue getDummyDV(String rmName){
        DataValue dv = dataValueMap.get(rmName);
        if(dv == null) {
            throw new IllegalArgumentException("unsupported RM class[" + rmName + "]");
        }else{
            return dv;
        }
    }

}
