package se.cambio.cds.util;

import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.*;
import org.openehr.rm.datatypes.quantity.datetime.*;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import org.openehr.rm.support.measurement.SimpleMeasurementService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cm.configuration.TerminologyServiceConfiguration;
import se.cambio.cm.controller.terminology.TerminologyService;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.misc.DataValueGenerator;

import java.math.BigDecimal;
import java.util.*;

import static java.lang.String.format;
import static org.apache.commons.lang.StringUtils.substringBefore;


public class DVUtil {

    private static Logger logger = LoggerFactory.getLogger(DVUtil.class);

    public static DataValue createDV(
            ElementInstance elementInstance, String rmName, String attributeName, Object value) {
        DataValue dv = elementInstance.getDataValue();
        if (dv == null) {
            dv = DataValueGenerator.getDummyDV(rmName);
        }
        return DataValueGenerator.createDV(dv, attributeName, value);
    }

    //Compares to DataValues ignoring language dependent labels (DvCodedText & DvOrdinal)
    public static boolean equalDVs(DataValue dv1, DataValue dv2) {
        if (dv1 instanceof DvCodedText && dv2 instanceof DvCodedText) {
            DvCodedText dvCodedText1 = (DvCodedText) dv1;
            DvCodedText dvCodedText2 = (DvCodedText) dv2;
            return dvCodedText1.getDefiningCode().equals(dvCodedText2.getDefiningCode()) && dvCodedText2.getTerminologyId().equals(dvCodedText2.getTerminologyId());
        } else if (dv1 instanceof DvOrdinal && dv2 instanceof DvOrdinal) {
            DvOrdinal dvOrdinal1 = (DvOrdinal) dv1;
            DvOrdinal dvOrdinal2 = (DvOrdinal) dv2;
            return dvOrdinal1.getValue() == dvOrdinal2.getValue() && equalDVs(dvOrdinal1.getSymbol(), dvOrdinal2.getSymbol());
        } else if (dv1 instanceof DvQuantity && dv2 instanceof DvQuantity) {
            DvQuantity dvQuantity1 = (DvQuantity) dv1;
            DvQuantity dvQuantity2 = (DvQuantity) dv2;
            int precision = Math.max(dvQuantity1.getPrecision(), dvQuantity2.getPrecision());
            double magnitude1 = round(dvQuantity1.getMagnitude(), precision);
            double magnitude2 = round(dvQuantity2.getMagnitude(), precision);
            return SimpleMeasurementService.getInstance().compare(dvQuantity1.getUnits(), magnitude1, dvQuantity2.getUnits(), magnitude2) == 0;
        } else if (dv1 instanceof DvProportion && dv2 instanceof DvProportion) {
            DvProportion dvProportion1 = (DvProportion) dv1;
            DvProportion dvProportion2 = (DvProportion) dv2;
            BigDecimal bigDecimalProportion1 = BigDecimal.valueOf(dvProportion1.getNumerator() / dvProportion1.getDenominator());
            BigDecimal bigDecimalProportion2 = BigDecimal.valueOf(dvProportion2.getNumerator() / dvProportion2.getDenominator());
            return bigDecimalProportion1.equals(bigDecimalProportion2);
        } else if (dv1 instanceof DvTemporal && dv2 instanceof DvTemporal) {
            DvTemporal dvTemporal1 = (DvTemporal) dv1;
            DvTemporal dvTemporal2 = (DvTemporal) dv2;
            return dvTemporal1.getDateTime().getMillis() == dvTemporal2.getDateTime().getMillis();
        } else {
            return dv1 == null && dv2 == null
                    || dv1 != null && dv1.equals(dv2);
        }
    }

    //Used by the drools engine
    public static boolean equalDV(boolean inPredicate, ElementInstance ei, DataValue dv2, boolean negated) {
        if (ei instanceof PredicateGeneratedElementInstance) {
            return inPredicate;
        } else {
            boolean result = DVUtil.equalDVs(ei.getDataValue(), dv2);
            if (negated) {
                return !result;
            } else {
                return result;
            }
        }
    }

    public static boolean nullValueEquals(DvCodedText nullFlavour, Object obj) {
        if (obj instanceof DvCodedText) {
            if (nullFlavour != null) {
                return DVUtil.equalDVs(nullFlavour, (DataValue) obj);
            } else {
                return false;
            }
        } else {
            return false;
        }
    }


    @Deprecated
    public static boolean isSubClassOf(
            boolean inPredicate, ElementInstance ei, Map<ElementInstance, Map<String, Boolean>> bindingsMap,
            DataValue... dataValues) {
        return isSubClassOf(inPredicate, ei, bindingsMap, null, dataValues);
    }

    public static boolean isSubClassOf(
            boolean inPredicate, ElementInstance ei, Map<ElementInstance, Map<String, Boolean>> bindingsMap,
            String bindReference, DataValue... dataValues) {
        return isSubClassOfCached(inPredicate, ei, bindingsMap, false, bindReference, dataValues);
    }

    public static boolean isSubClassOf(boolean inPredicate, ElementInstance ei, DataValue... dataValues) {
        if (!inPredicate && ei instanceof PredicateGeneratedElementInstance) {
            return false;
        } else {
            CodePhrase codePhrase = getCodePhrase(ei.getDataValue());
            Set<CodePhrase> codePhrases = new HashSet<>();
            for (DataValue dataValue : dataValues) {
                codePhrases.add(getCodePhrase(dataValue));
            }
            if (codePhrase != null && !codePhrases.isEmpty()) {
                return getTerminologyService().isSubclassOf(codePhrase, codePhrases);
            } else {
                return false;
            }
        }
    }

    public static boolean isSubClassOfCached(
            boolean inPredicate, ElementInstance ei, Map<ElementInstance, Map<String, Boolean>> bindingsMap,
            boolean negation, String bindReference, DataValue... dataValues) {
        Map<String, Boolean> bindingMapByElementInstance = bindingsMap.computeIfAbsent(ei, k -> new HashMap<>());
        String dataValueKey;
        if (bindReference != null) {
            dataValueKey = negation + bindReference;
        } else {
            dataValueKey = getDataValuesKey(dataValues, negation);
        }
        Boolean isSubClass = bindingMapByElementInstance.get(dataValueKey);
        if (isSubClass == null) {
            if (!negation) {
                isSubClass = isSubClassOf(inPredicate, ei, dataValues);
            } else {
                isSubClass = isNotSubClassOf(inPredicate, ei, dataValues);
            }
            bindingMapByElementInstance.put(dataValueKey, isSubClass);
        }
        return isSubClass;
    }

    private static String getDataValuesKey(DataValue[] dataValues, boolean negation) {
        StringBuilder sb = new StringBuilder();
        sb.append(negation).append(",");
        for (DataValue dataValue : dataValues) {
            if (dataValue instanceof DvCodedText) {
                DvCodedText codedText = (DvCodedText) dataValue;
                appendCodedTextKey(sb, codedText);
            } else if (dataValue instanceof DvOrdinal) {
                DvCodedText codedText = ((DvOrdinal) dataValue).getSymbol();
                appendCodedTextKey(sb, codedText);
            } else {
                sb.append(dataValue.serialise());
            }
        }
        return sb.toString();
    }

    private static void appendCodedTextKey(StringBuilder sb, DvCodedText codedText) {
        sb.append(codedText.getTerminologyId()).append("-").append(codedText.getCode()).append(",");
    }

    private static CodePhrase getCodePhrase(DataValue dv) {
        if (dv instanceof DvCodedText) {
            return ((DvCodedText) dv).getDefiningCode();
        } else if (dv instanceof DvOrdinal) {
            return ((DvOrdinal) dv).getSymbol().getDefiningCode();
        } else if (dv instanceof DvText) {
            try {
                DataValue dvAux = DataValue.parseValue(OpenEHRDataValues.DV_CODED_TEXT + "," + ((DvText) dv).getValue());
                if (dvAux instanceof DvCodedText) {
                    return ((DvCodedText) dvAux).getDefiningCode();
                } else {
                    return null;
                }
            } catch (Exception ex) {
                logger.warn("Unable to get CodePhrase from text '" + dv.toString() + "'");
                return null;
            }
        } else {
            return null;
        }
    }

    @Deprecated
    public static boolean isNotSubClassOf(
            boolean inPredicate, ElementInstance ei, Map<ElementInstance, Map<String, Boolean>> bindingsMap, DataValue... dataValues) {
        return isNotSubClassOf(inPredicate, ei, bindingsMap, null, dataValues);
    }

    public static boolean isNotSubClassOf(
            boolean inPredicate, ElementInstance ei, Map<ElementInstance, Map<String, Boolean>> bindingsMap, String bindReference, DataValue... dataValues) {
        return isSubClassOfCached(inPredicate, ei, bindingsMap, true, bindReference, dataValues);
    }

    public static boolean isNotSubClassOf(boolean inPredicate, ElementInstance ei, DataValue... dataValues) {
        if (ei instanceof PredicateGeneratedElementInstance) {
            return true;
        } else {
            CodePhrase codePhrase = getCodePhrase(ei.getDataValue());
            Set<CodePhrase> codePhrases = new HashSet<>();
            for (DataValue dataValue : dataValues) {
                codePhrases.add(getCodePhrase(dataValue));
            }
            if (codePhrase != null && !codePhrases.isEmpty()) {
                return !getTerminologyService().isSubclassOf(codePhrase, codePhrases);
            } else {
                return false;
            }
        }
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    public static int compareDVs(DataValue dv1, DataValue dv2) {
        if (dv1 instanceof DvText) {
            return dv1.equals(dv2) ? 0 : -1;
        } else {
            if (dv1 instanceof Comparable<?>) {
                return ((Comparable) dv1).compareTo(dv2);
            } else {
                return -1;
            }
        }
    }

    public static boolean compatibleComparison(DataValue dv1, DataValue dv2) {
        if (dv1 instanceof DvQuantity) {
            if (dv2 instanceof DvQuantity) {
                String unit1 = ((DvQuantity) dv1).getUnits();
                String unit2 = ((DvQuantity) dv2).getUnits();
                boolean compatible = false;
                try {
                    compatible = SimpleMeasurementService.getInstance().unitsComparable(unit1, unit2);
                } catch (IllegalArgumentException ex) {
                    logger.warn("Illegal argument comparing unit '" + unit1 + "' with '" + unit2 + "'");
                    return false;
                }
                if (!compatible) {
                    logger.warn("Comparing two elements with incompatible units '" + unit1 + "'!='" + unit2 + "'");
                }
                return compatible;
            } else {
                return false;
            }
        } else {
            return dv1 instanceof DvCount && dv2 instanceof DvCount
                    || dv1 instanceof DvTemporal<?> && dv2 instanceof DvTemporal<?>
                    || dv1 instanceof DvDuration && dv2 instanceof DvDuration
                    || dv1 instanceof DvProportion && dv2 instanceof DvProportion
                    || dv1 instanceof DvOrdinal && dv2 instanceof DvOrdinal;
        }
    }

    public static double round(double unroundedDouble, int precision) {
        BigDecimal bd = new BigDecimal(unroundedDouble);
        bd = bd.setScale(precision, BigDecimal.ROUND_HALF_UP);
        return bd.doubleValue();
    }

    public static ConstantExpression convertToExpression(DataValue dv) {
        String dataValueStr = dv.serialise();
        dataValueStr = dataValueStr.substring(dataValueStr.indexOf(",") + 1);
        if (dv instanceof DvCodedText) {
            DvCodedText dvCT = (DvCodedText) dv;
            return new CodedTextConstant(dvCT.getValue(), dvCT.getDefiningCode());
        } else if (dv instanceof DvOrdinal) {
            DvOrdinal dvOrdinal = (DvOrdinal) dv;
            return new OrdinalConstant(dvOrdinal);
        } else if (dv instanceof DvText) {
            return new StringConstant(dataValueStr);
        } else if (dv instanceof DvDateTime) {
            return new DateTimeConstant(getDateTimeStrWithoutMillisAndTimezone(dataValueStr));
        } else if (dv instanceof DvQuantity) {
            return new QuantityConstant((DvQuantity) dv);
        } else if (dv instanceof DvCount) {
            if (dataValueStr.trim().startsWith("-")) {
                dataValueStr = "(" + dataValueStr + ")";
            }
            return new ConstantExpression(dataValueStr);
        } else {
            return new ConstantExpression(dataValueStr);
        }
    }

    public static boolean checkMaxMin(DataValue originalDv, DataValue checkDv, String opSymbol, ArchetypeReference originalAr, ArchetypeReference checkAr) {
        if (!(originalDv instanceof DvOrdered) || !(checkDv instanceof DvOrdered)) {
            return false;
        }
        if (!areDomainsCompatible(originalAr.getIdDomain(), checkAr.getIdDomain())) {
            return false;
        }
        if (comparingGeneratedArWithRealAr(originalAr, checkAr)) {
            return true;
        }
        if (comparingRealArWithGeneratedAr(originalAr, checkAr)) {
            return false;
        }
        int comp = compare(originalDv, checkDv);
        if (OperatorKind.MAX.getSymbol().equals(opSymbol)) {
            return comp < 0;
        } else if (OperatorKind.MIN.getSymbol().equals(opSymbol)) {
            return comp > 0;
        } else {
            throw new IllegalArgumentException(format("Operator for predicate '%s' is not valid.", opSymbol));
        }
    }

    private static int compare(DataValue originalDv, DataValue checkDv) {
        return ((DvOrdered) originalDv).compareTo(checkDv);
    }

    private static boolean comparingGeneratedArWithRealAr(ArchetypeReference originalAr, ArchetypeReference checkAr) {
        return originalAr instanceof GeneratedArchetypeReference && !(checkAr instanceof GeneratedArchetypeReference);
    }

    private static boolean comparingRealArWithGeneratedAr(ArchetypeReference originalAr, ArchetypeReference checkAr) {
        return !(originalAr instanceof GeneratedArchetypeReference) && checkAr instanceof GeneratedArchetypeReference;
    }

    private static String getDateTimeStrWithoutMillisAndTimezone(String dateTimeDVStr) {
        //Ignore millis if found
        if (dateTimeDVStr.indexOf(".") > 0) {
            return dateTimeDVStr.substring(0, dateTimeDVStr.indexOf("."));
        } else if (dateTimeDVStr.indexOf("+") > 0) {
            return dateTimeDVStr.substring(0, dateTimeDVStr.indexOf("+"));
        } else if (dateTimeDVStr.indexOf("-") > 0) {
            return dateTimeDVStr.substring(0, dateTimeDVStr.indexOf("-"));
        } else {
            return dateTimeDVStr;
        }
    }

    private static boolean areDomainsCompatible(String domain1, String domain2) {
        return domain1 == null || domain2 == null || domain1.equals(domain2);
    }

    public static Double calculateDuration(String value, Object operationValue, String symbol) {
        if (operationValue instanceof DvQuantity) {
            return calculateDurationAgainstQuantity(value, (DvQuantity) operationValue, symbol);
        } else if (operationValue instanceof Number) {
            return calculateDurationAgainstNumber(value);
        } else if (operationValue instanceof String) {
            return calculateDurationAgainstStringDateTime(value, operationValue, symbol);
        } else if (operationValue instanceof DvTemporal) {
            return calculateDurationAgainstDvTemporal(value, (DvTemporal) operationValue, symbol);
        } else {
            throw new RuntimeException(format("Invalid duration value '%s' used.", operationValue));
        }
    }

    public static Double calculateDuration(String value, String symbol) {
        return calculateDurationAgainstDvTemporal(value, new DvDateTime(), symbol);
    }


    private static Double calculateDurationAgainstNumber(String value) {
        return getAmountInMillisFromQuantityString(value);
    }


    private static Double calculateDurationAgainstQuantity(String value, DvQuantity quantity, String symbol) {
        switch (symbol) {
            case "*":
                return quantity.getMagnitude() * getAmountInMillisFromQuantityString(value);
            case "/":
                return quantity.getMagnitude() / getAmountInMillisFromQuantityString(value);
            default:
                throw new IllegalArgumentException(format("Unknown symbol '%s' for expression operation ", symbol));
        }
    }

    static double getAmountInMillisFromQuantityString(String value) {
        String units = StringUtils.substringAfter(value, ",");
        Double amount = Double.parseDouble(substringBefore(value, ","));
        Double multiplier = getMillisMultiplierFromUcum(units);
        return amount * multiplier;
    }

    private static Double calculateDurationAgainstStringDateTime(String value, Object dateTimeString, String symbol) {
        DateTime dateTime = new DateTime(dateTimeString);
        return calculateDurationAgainstDateTime(value, dateTime, symbol);
    }

    static Double calculateDurationAgainstDvTemporal(String value, DvTemporal operationTemporal, String symbol) {
        DateTime dateTime = getDateTime(operationTemporal, value);
        return calculateDurationAgainstDateTime(value, dateTime, symbol);
    }

    static Double calculateDurationAgainstDateTime(String value, DateTime operationDateTime, String symbol) {
        String units = StringUtils.substringAfter(value, ",").trim();
        String minusSignIfSubtraction = "-".equals(symbol) ? "-" : "";
        String amount = minusSignIfSubtraction + substringBefore(value, ",");
        Calendar resultDateTime = new DateTime(operationDateTime).toGregorianCalendar();
        if (amount.contains(".")) {
            logger.warn(format("Invalid amount detected while doing date operations '%s'. Using double as integer.", amount));
            amount = substringBefore(amount, ".");
        }
        resultDateTime.add(ucumToCalendar(units), Integer.parseInt(amount));
        return (double) Math.abs(resultDateTime.getTimeInMillis() - operationDateTime.toGregorianCalendar().getTimeInMillis());
    }

    static DateTime getDateTime(DvTemporal operationDataValue, String value) {
        if (operationDataValue instanceof DvDateTime) {
            return operationDataValue.getDateTime();
        } else if (operationDataValue instanceof DvDate) {
            return operationDataValue.getDateTime();
        } else if (operationDataValue instanceof DvTime) {
            return operationDataValue.getDateTime();
        } else {
            if (operationDataValue == null) {
                throw new IllegalArgumentException(
                        format("Cannot use null data value to evaluate expression %s", value));
            } else {
                throw new IllegalArgumentException(
                        format("Cannot use data value with class %s to evaluate expression %s",
                                operationDataValue.getClass().getName(), value));
            }
        }
    }

    public static Long ucumToMilliseconds(DvQuantity quantity) {
        String units = quantity.getUnits();
        Double magnitude = quantity.getMagnitude();
        Double multiplier = getMillisMultiplierFromUcum(units);
        return (long) (multiplier * magnitude);
    }

    private static Double getMillisMultiplierFromUcum(String units) {
        Double multiplier;
        switch (units) {
            case "a":
                multiplier = 31556926000.0;
                break;
            case "mo":
                multiplier = 2629743830.0;
                break;
            case "wk":
                multiplier = 604800000.0;
                break;
            case "d":
                multiplier = 86400000.0;
                break;
            case "h":
                multiplier = 3600000.0;
                break;
            case "min":
                multiplier = 60000.0;
                break;
            case "s":
                multiplier = 1000.0;
                break;
            case "S":
                multiplier = 1.0;
                break;
            default:
                throw new IllegalArgumentException(format("Unknown time units '%s'", units));
        }
        return multiplier;
    }

    private static int ucumToCalendar(String ucumUnits) {
        switch (ucumUnits) {
            case "a":
                return Calendar.YEAR;
            case "mo":
                return Calendar.MONTH;
            case "wk":
                return Calendar.WEEK_OF_YEAR;
            case "d":
                return Calendar.DAY_OF_YEAR;
            case "h":
                return Calendar.HOUR;
            case "min":
                return Calendar.MINUTE;
            case "s":
                return Calendar.SECOND;
            case "S":
                return Calendar.MILLISECOND;
            default:
                throw new IllegalArgumentException(format("Unknown time units '%s'", ucumUnits));
        }
    }

    private static TerminologyService getTerminologyService() {
        return TerminologyServiceConfiguration.getTerminologyServiceInstance();
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