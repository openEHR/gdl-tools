package se.cambio.cds.util.export;

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
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.cds.util.DVUtil;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class DVDefSerializer {

    private static final Pattern clonePattern = Pattern.compile("\\.createDV\\([\\s]*\\$([^\\,\"]+),");
    private static final Pattern setLinePattern = Pattern.compile("\\.createDV\\([^\\,]+,[\\s]*\"(.*)\"\\)$");
    private static final Pattern assignationLinePattern = Pattern.compile("([^\\Q+-*/=\\E]+)([\\Q+-*/\\E]?)\\=\"\\+\\((.*)\\)\\+\"$");
    public static final Pattern dvReferencePattern = Pattern.compile("\\$([\\w]+)+\\Q.getDataValue()).get\\E([\\w]+)\\(\\)");
    public static final Pattern dvDefinitionPatternWithOutQuotes = Pattern.compile("new [a-zA-Z]+\\((.*)\\)$");
    public static final Pattern dvDefinitionPatternWithQuotes = Pattern.compile("new [a-zA-Z]+\\(\"(.*)\"\\)$");
    public static final Pattern dvDefinitionPattern = Pattern.compile("(new [a-zA-Z]+\\(.*\\))");
    public static final String commaSplitPatternOutsideParenthesis = ",(?![^(]*\\))";
    private static final Map<String, DataValue> dataValueMap;


    public static String getDVInstantiation(DataValue dataValue) {
        if (dataValue instanceof DvQuantity) {
            StringBuilder sb = new StringBuilder();
            sb.append("\"");
            sb.append(((DvQuantity) dataValue).getUnits());
            sb.append("\"");
            sb.append(",");
            sb.append(((DvQuantity) dataValue).getMagnitude());
            sb.append(",");
            sb.append(((DvQuantity) dataValue).getPrecision());
            return getDVInstantiationWithoutQuotes(DvQuantity.class.getSimpleName(), sb.toString());
        }
        if (dataValue instanceof DvDuration) {
            return getDVInstantiation(DvDuration.class.getSimpleName(), ((DvDuration) dataValue).getValue());
        }
        if (dataValue instanceof DvDateTime) {
            DvDateTime dvDateTime = (DvDateTime) dataValue;
            return getDVInstantiation(DvDateTime.class.getSimpleName(), dvDateTime.toString());
        }
        if (dataValue instanceof DvDate) {
            DvDate dvDate = (DvDate) dataValue;
            return getDVInstantiation(DvDate.class.getSimpleName(), dvDate.toString());
        }
        if (dataValue instanceof DvTime) {
            DvTime dvTime = (DvTime) dataValue;
            return getDVInstantiation(DvTime.class.getSimpleName(), dvTime.toString());
        }
        if (dataValue instanceof DvOrdinal) {
            DvOrdinal dvOrdinal = (DvOrdinal) dataValue;
            DvCodedText dvCodedText = dvOrdinal.getSymbol();
            StringBuilder sb = new StringBuilder();
            sb.append(dvOrdinal.getValue());
            sb.append(",\"");
            sb.append(dvCodedText.getValue());
            sb.append("\",\"");
            sb.append(dvCodedText.getDefiningCode().getTerminologyId().getValue());
            sb.append("\",\"");
            sb.append(dvCodedText.getDefiningCode().getCodeString());
            sb.append("\"");
            return getDVInstantiationWithoutQuotes(DvOrdinal.class.getSimpleName(), sb.toString());
        }
        if (dataValue instanceof DvCodedText) {
            DvCodedText dvCodedText = (DvCodedText) dataValue;
            StringBuilder sb = new StringBuilder();
            sb.append("\"");
            sb.append(dvCodedText.getValue());
            sb.append("\",\"");
            sb.append(dvCodedText.getDefiningCode().getTerminologyId().getValue());
            sb.append("\",\"");
            sb.append(dvCodedText.getDefiningCode().getCodeString());
            sb.append("\"");
            return getDVInstantiationWithoutQuotes(DvCodedText.class.getSimpleName(), sb.toString());
        }
        if (dataValue instanceof DvCount) {
            StringBuilder sb = new StringBuilder();
            sb.append(((DvCount) dataValue).getMagnitude());
            return getDVInstantiationWithoutQuotes(DvCount.class.getSimpleName(), sb.toString());
        }
        if (dataValue instanceof DvText) {
            return getDVInstantiation(DvText.class.getSimpleName(), ((DvText) dataValue).getValue());
        }
        if (dataValue instanceof DvBoolean) {
            StringBuilder sb = new StringBuilder();
            sb.append(((DvBoolean) dataValue).getValue());
            return getDVInstantiationWithoutQuotes(DvBoolean.class.getSimpleName(), sb.toString());
        }
        if (dataValue instanceof DvProportion) {
            DvProportion dvProportion = (DvProportion) dataValue;
            StringBuilder sb = new StringBuilder();
            sb.append(DVUtil.round(dvProportion.getNumerator(), dvProportion.getPrecision()));
            sb.append(",");
            sb.append(DVUtil.round(dvProportion.getDenominator(), dvProportion.getPrecision()));
            sb.append(",");
            sb.append(ProportionTypesConst.getInstance().getInstanceID(dvProportion.getType()));
            sb.append(",");
            sb.append(dvProportion.getPrecision());
            return getDVInstantiationWithoutQuotes(DvProportion.class.getSimpleName(), sb.toString());
        }
        if (dataValue instanceof DvList) {
            Collection<DataValue> dataValues = ((DvList) dataValue).getDataValues();
            StringBuilder sb = new StringBuilder();
            String prefix = "";
            for (DataValue dataValueAux : dataValues) {
                sb.append(prefix);
                sb.append(getDVInstantiation(dataValueAux));
                prefix = ", ";
            }
            return sb.toString();
        } else {
            throw new IllegalArgumentException("Unknown data value '" + dataValue.getClass().getSimpleName() + "'");
        }
    }

    public static String getDVInstantiation(String dvClassName, String dvDefinition) {
        if (dvDefinition.startsWith("'")) {
            dvDefinition = dvDefinition.substring(1, dvDefinition.length() - 1);
        }
        return "new " + dvClassName + "(\"" + dvDefinition + "\")";
    }

    public static String getDVInstantiationWithoutQuotes(String dvClassName, String dvDefinition) {
        return "new " + dvClassName + "(" + dvDefinition + ")";
    }

    public static String getDVDefinitionWithoutQuotes(String dvInstantiation) {
        if (dvInstantiation != null) {
            Matcher matcher = dvDefinitionPatternWithOutQuotes.matcher(dvInstantiation.trim());
            if (matcher.find()) {
                return matcher.group(1);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    public static String getDVDefinition(String dvInstantiation) {
        if (dvInstantiation != null) {
            Matcher matcher = dvDefinitionPatternWithQuotes.matcher(dvInstantiation.trim());
            if (matcher.find()) {
                return matcher.group(1);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    public static String getDVCloneInstanceName(String expression) {
        if (expression != null) {
            Matcher matcher = clonePattern.matcher(expression.trim());
            if (matcher.find()) {
                return matcher.group(1);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    public static boolean isDVExpression(String expression) {
        if (expression != null) {
            Matcher matcher = setLinePattern.matcher(expression.trim());
            if (matcher.find()) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    public static Calendar getCalendarFromDVDataTimeDef(String dvDefinition) {
        dvDefinition = dvDefinition.replace(":", "");
        DvDateTime dv = new DvDateTime(dvDefinition);
        Calendar cal = Calendar.getInstance();
        cal.set(
                dv.getYear(),
                dv.getMonth() - 1,
                dv.getDay(),
                dv.getHour(),
                dv.getMinute(),
                dv.getSecond());
        //TODO TIMEZONE
        return cal;
    }

    public static ArrayList<String[]> getDVAssignationsFromExpression(String expression) throws InternalErrorException {
        Matcher matcher = setLinePattern.matcher(expression.trim());
        if (matcher.find()) {
            return getDVAssignationsFromSetLine(matcher.group(1));
        } else {
            throw new InternalErrorException(new IllegalArgumentException("Could not parse: " + expression));
        }
    }

    public static ArrayList<String[]> getDVAssignationsFromSetLine(String setLine) throws InternalErrorException {
        ArrayList<String[]> setLines = new ArrayList<>();
        if (!setLine.trim().isEmpty()) {
            setLine = setLine.trim().substring(1, setLine.length());
            String[] assignationLines = setLine.split(",[\\s]*@");
            for (String assignationLine : assignationLines) {
                Matcher matcher = assignationLinePattern.matcher(assignationLine);
                if (matcher.find()) {
                    setLines.add(new String[]{matcher.group(1), matcher.group(2), matcher.group(3)});
                } else {
                    throw new InternalErrorException(new IllegalArgumentException("Could not parse: " + assignationLine));
                }
            }
        }
        return setLines;
    }


    public static String getReadableExpression(String expression) {
        ArrayList<String[]> assignations = getDVAssignationsFromExpression(expression);
        String instanceName = getDVCloneInstanceName(expression);
        if (assignations.isEmpty()) {
            return instanceName;
        } else {
            for (String[] assignation : assignations) {
                if ("magnitude".equals(assignation[0])) {
                    StringBuilder resulStr = new StringBuilder();
                    if (assignation[1] != null && !assignation[1].isEmpty()) {
                        resulStr.append(instanceName + assignation[1]);
                    }
                    resulStr.append(getExpressionWithReadableReferences(assignation[2]));
                    return resulStr.toString();
                }
            }
        }
        return OpenEHRLanguageManager.getMessage("Expression");
    }

    private static String getExpressionWithReadableReferences(String stringWithReferences) {
        Pattern regex = Pattern.compile("\\$([\\w]+)+\\Q.getDataValue()).get\\E([\\w]+)\\(\\)");
        Matcher matcher = regex.matcher(stringWithReferences);
        while (matcher.find()) {
            String handle = matcher.group(1);
            String field = matcher.group(2);
            String ref = "\\(\\([\\w]+\\)\\Q$" + handle + ".getDataValue()).get" + field + "()\\E";
            stringWithReferences = stringWithReferences.replaceAll(ref, handle);
        }
        return stringWithReferences;
    }

    public static String getCodeFromDVInstantiation(String dvInstantiation) {
        String dvDefinition = DVDefSerializer.getDVDefinitionWithoutQuotes(dvInstantiation);
        if (dvDefinition != null && dvDefinition.contains(",")) {
            String[] splittedDVDefinition = dvDefinition.split(",");
            dvDefinition = splittedDVDefinition[splittedDVDefinition.length - 1];
            dvDefinition = dvDefinition.replace("\"", "");
            return dvDefinition;
        } else {
            return null;
        }
    }

    public static String getDVClassName(String rmName) {
        return getDummyDV(rmName).getClass().getSimpleName();
    }

    public static DataValue getDummyDV(String rmName) {
        DataValue dv = dataValueMap.get(rmName);
        if (dv == null) {
            throw new IllegalArgumentException("unsupported RM class[" + rmName + "]");
        } else {
            return dv;
        }
    }

    public static String getReadableValue(DataValue dv, TermDefinition termDefinition) {
        if (dv instanceof DvCodedText) {
            CodePhrase cp = ((DvCodedText) dv).getDefiningCode();
            Term term = getTerm(termDefinition, cp);
            if (term != null) {
                return term.getText();
            }
            return ((DvCodedText) dv).getValue();
        } else if (dv instanceof DvOrdinal) {
            CodePhrase cp = ((DvOrdinal) dv).getSymbol().getDefiningCode();
            Term term = getTerm(termDefinition, cp);
            if (term != null) {
                return term.getText();
            } else {
                return ((DvOrdinal) dv).getSymbol().getValue();
            }
        } else if (dv instanceof DvQuantity) {
            DvQuantity dvQuantity = ((DvQuantity) dv);
            DecimalFormat format = getDecimalFormat(dvQuantity.getPrecision());
            return format.format(dvQuantity.getMagnitude()) + " " + dvQuantity.getUnits();
        } else if (dv instanceof DvProportion) {
            DvProportion dvProportion = ((DvProportion) dv);
            DecimalFormat format = getDecimalFormat(dvProportion.getPrecision());
            return format.format(dvProportion.getNumerator()) + "/" + format.format(dvProportion.getDenominator());
        } else if (dv instanceof DvDateTime) {
            DateFormat df = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT);
            Date date = ((DvDateTime) dv).getDateTime().toDate();
            return df.format(date);
        } else if (dv instanceof DvDate) {
            DateFormat df = DateFormat.getDateInstance(DateFormat.SHORT);
            Date date = ((DvDate) dv).getDateTime().toDate();
            return df.format(date);
        } else if (dv instanceof DvTime) {
            DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
            Date date = ((DvTime) dv).getDateTime().toDate();
            return df.format(date);
        } else if (dv instanceof DvText) {
            return "\"" + dv.toString() + "\"";
        } else if (dv != null) {
            return dv.toString();
        } else {
            return null;
        }
    }

    private static Term getTerm(TermDefinition termDefinition, CodePhrase cp) {
        if (cp.getTerminologyId().getValue().equals(OpenEHRConst.LOCAL)) {
            if (termDefinition != null) {
                return termDefinition.getTerms().get(cp.getCodeString());
            }
        }
        return null;
    }

    private static DecimalFormat getDecimalFormat(int precision) {
        return OpenEHRNumberFormat.getDecimalFormat(precision);
    }

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
        dataValueMap.put(OpenEHRDataValues.DV_PROPORTION, new DvProportion(1, 1, ProportionKind.UNITARY, 0));
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