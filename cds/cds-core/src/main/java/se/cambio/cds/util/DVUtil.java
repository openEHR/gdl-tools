package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.basic.DvBoolean;
import org.openehr.rm.datatypes.quantity.*;
import org.openehr.rm.datatypes.quantity.datetime.*;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import org.openehr.rm.support.measurement.SimpleMeasurementService;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.InvalidCodeException;
import se.cambio.openehr.util.exceptions.UnsupportedTerminologyException;

import java.math.BigDecimal;
import java.util.*;


public class DVUtil {

    public static DataValue createDV(ElementInstance elementInstance, String rmName, String attributeName, Object value) throws InternalErrorException{
        DataValue dv = elementInstance.getDataValue();
        if(dv==null){
            dv = getDummyDV(rmName);
        }
        return createDV(dv, attributeName, value);
    }

    public static DataValue createDV(DataValue dataValue, String attributeName, Object value) throws InternalErrorException{
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
            throw new IllegalArgumentException("Unknow data value '"+dataValue.getClass().getSimpleName()+"'");
        }
    }

    public static DvQuantity create(DvQuantity dvQuantity, String attributeName, Object value) throws InternalErrorException{

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

    public static DvDuration create(DvDuration dvDuration, String attributeName, Object value) throws InternalErrorException{

        String durationValue = dvDuration.getValue();

        if (attributeName.equals("value")){
            durationValue = (String)value;
        }

        return new DvDuration(durationValue);
    }

    public static DvDateTime create(DvDateTime dvDateTime, String attributeName, Object value) throws InternalErrorException{

        Calendar cal = Calendar.getInstance();
        setCalendar(cal, Calendar.YEAR, attributeName, (Integer)value, "year", dvDateTime.getYear());
        setCalendar(cal, Calendar.MONTH, attributeName, (Integer)value, "month", dvDateTime.getMonth());
        setCalendar(cal, Calendar.DATE, attributeName, (Integer)value, "day", dvDateTime.getDay());
        setCalendar(cal, Calendar.HOUR, attributeName, (Integer)value, "hour", dvDateTime.getHour());
        setCalendar(cal, Calendar.MINUTE, attributeName, (Integer)value, "minute", dvDateTime.getMinute());
        setCalendar(cal, Calendar.SECOND, attributeName, (Integer)value, "second", dvDateTime.getSecond());

        //TODO "fractionalSecond";
        //TODO "timeZone";

        TimeZone timeZone =  Calendar.getInstance().getTimeZone();
        return new DvDateTime(
                cal.get(Calendar.YEAR),
                cal.get(Calendar.MONTH),
                cal.get(Calendar.DATE),
                cal.get(Calendar.HOUR),
                cal.get(Calendar.MINUTE),
                cal.get(Calendar.SECOND), /*fractionalSecond,*/ timeZone);
    }

    public static DvDate create(DvDate dvDate, String attributeName, Object value) throws InternalErrorException{
        Calendar cal = Calendar.getInstance();
        setCalendar(cal, Calendar.YEAR, attributeName, (Integer)value, "year", dvDate.getYear());
        setCalendar(cal, Calendar.MONTH, attributeName, (Integer)value, "month", dvDate.getMonth());
        setCalendar(cal, Calendar.DATE, attributeName, (Integer)value, "day", dvDate.getDay());
        return new DvDate(
                cal.get(Calendar.HOUR),
                cal.get(Calendar.MINUTE),
                cal.get(Calendar.SECOND));
    }

    public static DvCount create(DvCount dvCount, String attributeName, Object value) throws InternalErrorException{
        int magnitude = dvCount.getMagnitude();
        if(attributeName.equals("magnitude")){
            if (value instanceof Integer){
                magnitude =((Integer)value);
            }else if (value instanceof Double){
                magnitude = ((Double)value).intValue();
            }else{
                Logger.getLogger(DVUtil.class).warn("Unkown class for count: "+value.getClass().getName());
            }
        }
        return new DvCount(magnitude);
    }

    public static DvCodedText create(DvCodedText dvCodedText, String expressi, String attributeName, Object value) throws InternalErrorException{
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

    public static DvText create(DvText dvText, String attributeName, Object value) throws InternalErrorException{
        String textValue = dvText.getValue();
        if (attributeName.equals("value")){
            textValue = (String)value;;
        }

        return new DvText(textValue);
    }

    public static DvOrdinal create(DvOrdinal dvOrdinal, String attributeName, Object value) throws InternalErrorException{
        Integer ordinalValue = dvOrdinal.getValue();
        if (attributeName.equals("value")){
            ordinalValue = (Integer)value;
        }
        return new DvOrdinal(ordinalValue, dvOrdinal.getSymbol());
    }

    public static DvTime create(DvTime dvTime, String attributeName, Object value) throws InternalErrorException{
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
                cal.get(Calendar.YEAR),
                cal.get(Calendar.MONTH),
                cal.get(Calendar.DATE), /*fractionalSecond,*/ timeZone);
    }

    public static DvBoolean create(DvBoolean dvBoolean, String attributeName, Object value) throws InternalErrorException{
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

    //Compares to DataValues ignoring language dependent labels (DvCodedText & DvOrdinal)
    public static boolean equalDVs(DataValue dv1, DataValue dv2){
        if (dv1 instanceof DvCodedText && dv2 instanceof DvCodedText){
            return ((DvCodedText)dv1).getDefiningCode().equals(((DvCodedText)dv2).getDefiningCode());
        }else if (dv1 instanceof DvOrdinal && dv2 instanceof DvOrdinal){
            DvOrdinal dvOrdinal1 = (DvOrdinal) dv1;
            DvOrdinal dvOrdinal2 = (DvOrdinal) dv2;
            return dvOrdinal1.getValue()==dvOrdinal2.getValue() && dvOrdinal1.getSymbol().getDefiningCode().equals(dvOrdinal2.getSymbol().getDefiningCode());
        }else if (dv1 instanceof DvQuantity && dv2 instanceof DvQuantity){
            DvQuantity dvQuantity1 = (DvQuantity) dv1;
            DvQuantity dvQuantity2 = (DvQuantity) dv2;
            int precision = Math.max(dvQuantity1.getPrecision(), dvQuantity2.getPrecision());
            double magnitude1 = round(dvQuantity1.getMagnitude(), precision);
            double magnitude2 = round(dvQuantity2.getMagnitude(), precision);
            return SimpleMeasurementService.getInstance().compare(dvQuantity1.getUnits(), magnitude1, dvQuantity2.getUnits(), magnitude2)==0;
        }else if (dv1 instanceof DvProportion && dv2 instanceof DvProportion){
            DvProportion dvProportion1 = (DvProportion) dv1;
            DvProportion dvProportion2 = (DvProportion) dv2;
            return (dvProportion1.getNumerator()/dvProportion1.getDenominator())==(dvProportion2.getNumerator()/dvProportion2.getDenominator());
        }else{
            if (dv1==null && dv2==null){
                return true;
            }else{
                if (dv1!=null){
                    return dv1.equals(dv2);
                }else{
                    return false;
                }
            }
        }
    }

    public static boolean equalDV(boolean inPredicate, ElementInstance ei, DataValue dv2) {
        if (!inPredicate && ei instanceof PredicateGeneratedElementInstance){
            return false;
        }else{
            if (dv2!=null){
                if (ei.getDataValue()!=null){
                    return DVUtil.equalDVs(ei.getDataValue(), dv2);
                }else{
                    return false;
                }
            }else{
                return false;
            }
        }
    }

    public static boolean nullValueEquals(DvCodedText nullFlavour, Object o) {
        if (o instanceof DvCodedText){
            if (nullFlavour!=null){
                return DVUtil.equalDVs(nullFlavour, (DataValue)o);
            }else{
                return false;
            }
        }else{
            return false;
        }
    }


    public static boolean isSubClassOf(boolean inPredicate, ElementInstance ei, DataValue... dataValues) {
        if (!inPredicate && ei instanceof PredicateGeneratedElementInstance){
            return false;
        }else{
            CodePhrase a = getCodePhrase(ei.getDataValue());
            Set<CodePhrase>  codePhrases = new HashSet<CodePhrase>();
            for (int i = 0; i < dataValues.length; i++) {
                codePhrases.add(getCodePhrase(dataValues[i]));
            }
            if (a!=null && !codePhrases.isEmpty()){
                try {
                    boolean result= OpenEHRSessionManager.getTerminologyFacadeDelegate().isSubclassOf(a, codePhrases);
                    return result;
                } catch (UnsupportedTerminologyException e) {
                    Logger.getLogger(DVUtil.class).warn("Unsuported terminolody '"+a.getTerminologyId()+"'");
                    return false;
                } catch (InvalidCodeException e) {
                    Logger.getLogger(DVUtil.class).warn("Invalid code : '"+a.getCodeString()+"'");
                    return false;
                }
            }else{
                return false;
            }
        }
    }

    private static CodePhrase getCodePhrase(DataValue dv){
        if (dv instanceof DvCodedText){
            return ((DvCodedText)dv).getDefiningCode();
        }else if (dv instanceof DvOrdinal){
            return ((DvOrdinal)dv).getSymbol().getDefiningCode();
        }else if (dv instanceof DvText){
            try{
                DataValue dvAux = DataValue.parseValue(OpenEHRDataValues.DV_CODED_TEXT+","+((DvText)dv).getValue());
                if (dvAux instanceof DvCodedText){
                    return ((DvCodedText)dvAux).getDefiningCode();
                }else{
                    return null;
                }
            }catch(Exception e){
                Logger.getLogger(DVUtil.class).warn("Unable to get CodePhrase from text '"+dv.toString()+"'");
                return null;
            }
        }else{
            return null;
        }
    }

    public boolean isNotSubClassOf(boolean inPredicate, ElementInstance ei, DataValue... dataValues){
        if (ei instanceof PredicateGeneratedElementInstance){
            return true;
        }else{
            //TODO Remove, exceptions should be handled
            CodePhrase a = getCodePhrase(ei.getDataValue());
            Set<CodePhrase>  codePhrases = new HashSet<CodePhrase>();
            for (int i = 0; i < dataValues.length; i++) {
                codePhrases.add(getCodePhrase(dataValues[i]));
            }
            if (a!=null && !codePhrases.isEmpty()){
                try {
                    return !OpenEHRSessionManager.getTerminologyFacadeDelegate().isSubclassOf(a, codePhrases);
                } catch (UnsupportedTerminologyException e) {
                    //TODO Remove, exceptions should be handled
                    return false;
                } catch (InvalidCodeException e) {
                    //TODO Remove, exceptions should be handled
                    return false;
                }
            }else{
                return false;
            }
        }
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static int compareDVs(DataValue dv1, DataValue dv2){
        if (dv1 instanceof DvText){
            return dv1.equals(dv2)?0:-1;
        }else{
            if (dv1 instanceof Comparable<?>){
                return ((Comparable)dv1).compareTo(dv2);
            }else{
                return -1;
            }
        }
    }

    public static boolean compatibleComparison(DataValue dv1, DataValue dv2){
        if (dv1 instanceof DvQuantity){
            if (dv2 instanceof DvQuantity){
                String unit1 = ((DvQuantity)dv1).getUnits();
                String unit2 = ((DvQuantity)dv2).getUnits();
                boolean compatible = false;
                try{
                    compatible = SimpleMeasurementService.getInstance().unitsComparable(unit1, unit2);
                }catch(IllegalArgumentException e){
                    Logger.getLogger(DVUtil.class).warn("Illegal argument comparing unit '"+unit1+"' with '"+unit2+"'");
                    return false;
                }
                if (!compatible){
                    Logger.getLogger(DVUtil.class).warn("Comparing two elements with incompatible units '"+unit1+"'!='"+unit2+"'");
                }
                return compatible;
            }else{
                return false;
            }
        }else if (dv1 instanceof DvCount && dv2 instanceof DvCount){
            return true;
        }else if (dv1 instanceof DvTemporal<?> && dv2 instanceof DvTemporal<?>){
            return true;
        }else if (dv1 instanceof DvDuration && dv2 instanceof DvDuration){
            return true;
        }else if (dv1 instanceof DvProportion && dv2 instanceof DvProportion){
            return true;
        }else if (dv1 instanceof DvOrdinal && dv2 instanceof DvOrdinal){
            return true;
        } else {
            return false; //Comparison of DVText always incompatible (not for equals/unequals)
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

    public static double round(double unroundedDouble, int precision){
        BigDecimal bd = new BigDecimal(unroundedDouble);
        bd = bd.setScale(precision,BigDecimal.ROUND_HALF_UP);
        return  bd.doubleValue();
    }

    public static ConstantExpression convertToExpression(DataValue dv){
        String dataValueStr = dv.serialise();
        dataValueStr = dataValueStr.substring(dataValueStr.indexOf(",")+1);
        if (dv instanceof DvText
                && !(dv instanceof DvCodedText
                || dv instanceof DvOrdinal)){
            return new StringConstant(dataValueStr);
        }else if (dv instanceof DvDateTime) {
            return new DateTimeConstant(dataValueStr);
        }else if (dv instanceof DvQuantity) {
            return new QuantityConstant((DvQuantity)dv);
        }else if (dv instanceof DvCodedText) {
            DvCodedText dvCT = (DvCodedText)dv;
            return new CodedTextConstant(dvCT.getValue(), dvCT.getDefiningCode());
        }else if (dv instanceof DvCodedText) {
            DvOrdinal dvOrdinal = (DvOrdinal)dv;
            return new OrdinalConstant(dvOrdinal);
        }else{
            return new ConstantExpression(dataValueStr);
        }
    }

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
        dataValueMap.put(OpenEHRDataValues.DV_PROPORTION, new DvProportion(1,1,ProportionKind.UNITARY,0));
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */