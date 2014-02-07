package se.cambio.cds.util;

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
import se.cambio.openehr.controller.session.data.CodedTexts;
import se.cambio.openehr.controller.session.data.Ordinals;
import se.cambio.openehr.controller.session.data.ProportionTypesUI;
import se.cambio.openehr.model.archetype.vo.CodedTextVO;
import se.cambio.openehr.util.*;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class DVDefSerializer {

    public static String VARTOKEN = "$";
    private static Pattern clonePattern = Pattern.compile("\\.createDV\\([\\s]*\\$([^\\,\"]+),");
    private static Pattern setLinePattern = Pattern.compile("\\.createDV\\([^\\,]+,[\\s]*\"(.*)\"\\)$");
    private static Pattern assignationLinePattern = Pattern.compile("([^\\Q+-*/=\\E]+)([\\Q+-*/\\E]?)\\=\"\\+\\((.*)\\)\\+\"$");
    public static Pattern dvReferencePattern = Pattern.compile("\\$([\\w]+)+\\Q.getDataValue()).get\\E([\\w]+)\\(\\)");
    public static Pattern dvDefinitionPatternWithOutQuotes = Pattern.compile("new [a-zA-Z]+\\((.*)\\)$");
    public static Pattern dvDefinitionPatternWithQuotes = Pattern.compile("new [a-zA-Z]+\\(\"(.*)\"\\)$");
    public static Pattern dvDefinitionPattern = Pattern.compile("(new [a-zA-Z]+\\(.*\\))");
    public static String commaSplitPatternOutsideParenthesis = ",(?![^(]*\\))";


    public static String getDVInstantiation(DataValue dataValue){
        if (dataValue instanceof DvQuantity){
            return getDVInstantiationWithoutQuotes(
                    DvQuantity.class.getSimpleName(),
                    "\""+((DvQuantity)dataValue).getUnits()+"\","+
                            ((DvQuantity)dataValue).getMagnitude()+","+
                            ((DvQuantity)dataValue).getPrecision());
        }if (dataValue instanceof DvDuration){
            return getDVInstantiation(DvDuration.class.getSimpleName(), ((DvDuration)dataValue).getValue());
        }if (dataValue instanceof DvDateTime){
            DvDateTime dvDateTime = (DvDateTime)dataValue;
            return getDVInstantiation(DvDateTime.class.getSimpleName(), dvDateTime.toString());
        }if (dataValue instanceof DvDate){
            DvDate dvDate = (DvDate)dataValue;
            return getDVInstantiation(DvDate.class.getSimpleName(), dvDate.toString());
        }if (dataValue instanceof DvTime){
            DvTime dvTime = (DvTime)dataValue;
            return getDVInstantiation(DvTime.class.getSimpleName(), dvTime.toString());
        }if (dataValue instanceof DvOrdinal){
            DvOrdinal dvOrdinal = (DvOrdinal)dataValue;
            DvCodedText dvCodedText = dvOrdinal.getSymbol();
            return getDVInstantiationWithoutQuotes(
                    DvOrdinal.class.getSimpleName(),
                    dvOrdinal.getValue()+",\""+
                            dvCodedText.getValue()+"\",\""+
                            dvCodedText.getDefiningCode().getTerminologyId().getValue()+"\",\""+
                            dvCodedText.getDefiningCode().getCodeString()+"\"");
        }if (dataValue instanceof DvCodedText){
            DvCodedText dvCodedText = (DvCodedText)dataValue;
            return getDVInstantiationWithoutQuotes(
                    DvCodedText.class.getSimpleName(), "\""+
                    dvCodedText.getValue()+"\",\""+
                    dvCodedText.getDefiningCode().getTerminologyId().getValue()+"\",\""+
                    dvCodedText.getDefiningCode().getCodeString()+"\"");
        }if (dataValue instanceof DvCount){
            return getDVInstantiationWithoutQuotes(DvCount.class.getSimpleName(), ""+((DvCount)dataValue).getMagnitude());
        }if (dataValue instanceof DvText){
            return getDVInstantiation(DvText.class.getSimpleName(), ((DvText)dataValue).getValue());
        }if (dataValue instanceof DvBoolean){
            return getDVInstantiationWithoutQuotes(DvBoolean.class.getSimpleName(), ""+((DvBoolean) dataValue).getValue());
        }if (dataValue instanceof DvProportion){
            DvProportion dvProportion = (DvProportion) dataValue;
            return getDVInstantiationWithoutQuotes(
                    DvProportion.class.getSimpleName(),
                    ""+DVUtil.round(dvProportion.getNumerator(), dvProportion.getPrecision())+","+
                            DVUtil.round(dvProportion.getDenominator(), dvProportion.getPrecision())+","+
                            ProportionTypesUI.getInstanceID(dvProportion.getType())+","+
                            dvProportion.getPrecision());
        }if (dataValue instanceof DvList){
            Collection<DataValue> dataValues = ((DvList)dataValue).getDataValues();
            StringBuffer sb = new StringBuffer();
            int i = 0;
            for (DataValue dataValueAux : dataValues) {
                sb.append(getDVInstantiation(dataValueAux));
                i++;
                if (i<dataValues.size()){
                    sb.append(", ");
                }
            }
            return sb.toString();
        }else{
            throw new IllegalArgumentException("Unknow data value '"+dataValue.getClass().getSimpleName()+"'");
        }
    }

    public static String getDVInstantiation(String dvClassName, String dvDefinition){
        if (dvDefinition.startsWith("'")){
            dvDefinition = dvDefinition.substring(1, dvDefinition.length()-1);
        }
        return "new "+dvClassName+"(\""+dvDefinition+"\")";
    }

    public static String getDVInstantiationWithoutQuotes(String dvClassName, String dvDefinition){
        return "new "+dvClassName+"("+dvDefinition+")";
    }

    public static String getDVDefinitionWithOutQuotes(String dvInstantiation){
        if (dvInstantiation!=null){
            Matcher m = dvDefinitionPatternWithOutQuotes.matcher(dvInstantiation.trim());
            if(m.find()){
                return m.group(1);
            }else{
                return null;
            }
        }else{
            return null;
        }
    }
    public static String getDVDefinition(String dvInstantiation){
        if (dvInstantiation!=null){
            Matcher m = dvDefinitionPatternWithQuotes.matcher(dvInstantiation.trim());
            if(m.find()){
                return m.group(1);
            }else{
                return null;
            }
        }else{
            return null;
        }
    }

    public static String getDVCloneInstanceName(String expression){
        if (expression!=null){
            Matcher m = clonePattern.matcher(expression.trim());
            if(m.find()){
                return m.group(1);
            }else{
                return null;
            }
        }else{
            return null;
        }
    }

    public static boolean isDVExpression(String expression){
        if (expression!=null){
            Matcher m = setLinePattern.matcher(expression.trim());
            if(m.find()){
                return true;
            }else{
                return false;
            }
        }else{
            return false;
        }
    }

    public static Calendar getCalendarFromDVDataTimeDef(String dvDefinition){
        dvDefinition = dvDefinition.replace(":", "");
        DvDateTime dv = new DvDateTime(dvDefinition);
        Calendar cal = Calendar.getInstance();
        cal.set(
                dv.getYear(),
                dv.getMonth()-1,
                dv.getDay(),
                dv.getHour(),
                dv.getMinute(),
                dv.getSecond());
        //TODO TIMEZONE
        return cal;
    }

    public static ArrayList<String[]> getDVAssignationsFromExpression(String expression) throws InternalErrorException{
        Matcher m = setLinePattern.matcher(expression.trim());
        if(m.find()){
            return getDVAssignationsFromSetLine(m.group(1));
        }else{
            throw new InternalErrorException(new IllegalArgumentException("Could not parse: "+expression));
        }
    }

    public static ArrayList<String[]> getDVAssignationsFromSetLine(String setLine) throws InternalErrorException{
        ArrayList<String[]> setLines = new ArrayList<String[]>();
        if(!setLine.trim().isEmpty()){
            setLine = setLine.trim().substring(1, setLine.length());
            String[] assignationLines = setLine.split(",[\\s]*@");
            for (String assignationLine : assignationLines) {
                Matcher m = assignationLinePattern.matcher(assignationLine);
                if(m.find()){
                    setLines.add(new String[]{m.group(1), m.group(2), m.group(3)});
                }else{
                    throw new InternalErrorException(new IllegalArgumentException("Could not parse: "+assignationLine));
                }
            }
        }
        return setLines;
    }


    public static String getReadableExpression(String expression) {
        try{
            ArrayList<String[]> assignations = getDVAssignationsFromExpression(expression);
            String instanceName = getDVCloneInstanceName(expression);
            if (assignations.isEmpty()){
                return instanceName;
            }else{
                for (String[] assignation : assignations) {
                    if ("magnitude".equals(assignation[0])){
                        StringBuffer resulStr = new StringBuffer();
                        if (assignation[1]!=null&&!assignation[1].isEmpty()){
                            resulStr.append(instanceName+assignation[1]);
                        }
                        resulStr.append(getExpressionWithReadableReferences(assignation[2]));
                        return resulStr.toString();
                    }
                }
            }
        }catch(InternalErrorException e){
            ExceptionHandler.handle(e);
        }
        return OpenEHRLanguageManager.getMessage("Expression");
    }

    private static String getExpressionWithReadableReferences(String stringWithReferences){
        Pattern regex = Pattern.compile("\\$([\\w]+)+\\Q.getDataValue()).get\\E([\\w]+)\\(\\)");
        Matcher m = regex.matcher(stringWithReferences);
        while (m.find()){
            String handle = m.group(1);
            String field = m.group(2);
            String ref = "\\(\\([\\w]+\\)\\Q$"+handle+".getDataValue()).get"+field+"()\\E";
            stringWithReferences = stringWithReferences.replaceAll(ref, handle);
        }
        return stringWithReferences;
    }

    public static String getReadableDefinition(String idTemplate, String idElement, String rmName, String dvInstantiation){
        if (OpenEHRDataValues.DV_CODED_TEXT.equals(rmName)){
            String codedTextName = getCodedTextNameFromDVInstantiation(idTemplate, idElement, dvInstantiation);
            if (codedTextName!=null){
                return codedTextName;
            }
        }else if (OpenEHRDataValues.DV_ORDINAL.equals(rmName)){
            String ordinalName = getOrdinalNameFromDVInstantiation(idTemplate, idElement, dvInstantiation);
            if (ordinalName!=null){
                return ordinalName;
            }
        }else if (OpenEHRDataValues.DV_COUNT.equals(rmName) ||
                OpenEHRDataValues.DV_BOOLEAN.equals(rmName)){
            String dvDefinition = getDVDefinitionWithOutQuotes(dvInstantiation);
            if (dvDefinition!=null){
                return dvDefinition;
            }
        }else if(OpenEHRDataValues.DV_QUANTITY.equals(rmName)){
            String dvDefinition = getDVDefinitionWithOutQuotes(dvInstantiation);
            if (dvDefinition!=null){
                String[] splitDef = dvDefinition.split("\\,");
                if (splitDef.length>2){
                    return  roundToStr(Double.parseDouble(splitDef[1]), Integer.parseInt(splitDef[2]))+" "+splitDef[0].replace("\"", "");
                }else{
                    return dvDefinition;
                }
            }
        }else if(OpenEHRDataValues.DV_PROPORTION.equals(rmName)){
            String dvDefinition = getDVDefinitionWithOutQuotes(dvInstantiation);
            if (dvDefinition!=null){
                String[] splitDef = dvDefinition.split("\\,");
                if (splitDef.length>3){
                    //TODO Display changes depending on proportion kind (slitDef[2])
                    return roundToStr(Double.parseDouble(splitDef[0]), Integer.parseInt(splitDef[3]))+
                            "/"+
                            roundToStr(Double.parseDouble(splitDef[1]), Integer.parseInt(splitDef[3]));
                }else{
                    return dvDefinition;
                }
            }
        }else if (OpenEHRDataValues.DV_DATE.equals(rmName)){
            return getDVDefinition(dvInstantiation);
        }else if (OpenEHRDataValues.DV_TIME.equals(rmName)){
            String dvDefinition = getDVDefinition(dvInstantiation);
            String[] defSplit = dvDefinition.split("\\+");
            if (defSplit.length>1){
                return defSplit[0];
            }else{
                return dvDefinition;
            }
        }else if(OpenEHRDataValues.DV_DATE_TIME.equals(rmName)){
            String dvDefinition = getDVDefinition(dvInstantiation);
            String[] defSplit = dvDefinition.split("\\+");
            if (defSplit.length>1){
                dvDefinition = defSplit[0];
            }
            defSplit = dvDefinition.split("T");
            String dateDefinition = null;
            String hourDefinition = null;
            if (defSplit.length>1){
                dateDefinition = defSplit[0];
                hourDefinition = defSplit[1];
            }else{
                dateDefinition = dvDefinition;
            }

            String result = "";
            if (dateDefinition!=null && !dateDefinition.isEmpty()){
                dateDefinition = dateDefinition.replaceAll("-", "");
                String year = dateDefinition.substring(0,4);
                int monthInt = (Integer.parseInt(dateDefinition.substring(4,6)));
                String month = (monthInt>9?"":"0")+monthInt;
                int dayInt = (Integer.parseInt(dateDefinition.substring(6,8)));
                String day = (dayInt>9?"":"0")+dayInt;
                result = result+day+"/"+month+"/"+year;
            }
            if (hourDefinition!=null){
                result = result+" "+hourDefinition;
            }
            return result;
        }
        String dvDefinition = getDVDefinitionWithOutQuotes(dvInstantiation);
        if (dvDefinition!=null){
            return dvDefinition;
        }else{
            return dvInstantiation;
        }
    }

    public static String getOrdinalNameFromDVInstantiation(String idTemplate, String idParentArchetypeNode, String dvInstantiation){
        return Ordinals.getOrdinalVO(idTemplate, idParentArchetypeNode, Integer.parseInt(getValueFromDVInstantiation(dvInstantiation))).getName();
    }

    public static String getValueFromDVInstantiation(String dvInstantiation){
        String dvDefinition = DVDefSerializer.getDVDefinitionWithOutQuotes(dvInstantiation);
        if (dvDefinition!=null && dvDefinition.contains(",")){
            dvDefinition = dvDefinition.split(",")[0];
            return dvDefinition;
        }else{
            return null;
        }
    }

    public static String getCodedTextNameFromDVInstantiation(String idTemplate, String idParentArchetypeNode, String dvInstantiation){
        CodedTextVO codedTexTVO = CodedTexts.getCodedTextVO(idTemplate, idParentArchetypeNode, getCodeFromDVInstantiation(dvInstantiation));
        if (codedTexTVO!=null){
            return codedTexTVO.getName();
        }else{
            return getCodeFromDVInstantiation(dvInstantiation);
        }
    }

    public static String getCodeFromDVInstantiation(String dvInstantiation){
        String dvDefinition = DVDefSerializer.getDVDefinitionWithOutQuotes(dvInstantiation);
        if (dvDefinition!=null && dvDefinition.contains(",")){
            String[] splittedDVDefinition = dvDefinition.split(",");
            dvDefinition = splittedDVDefinition[splittedDVDefinition.length-1];
            dvDefinition = dvDefinition.replace("\"", "");
            return dvDefinition;
        }else{
            return null;
        }
    }

    public static Collection<String> getCodesFromDVInstantiation(String dvInstantiation){
        if (dvInstantiation!=null){
            Collection<String> codes = new ArrayList<String>();
            String[] dvStrings = dvInstantiation.split(commaSplitPatternOutsideParenthesis);
            for (String dvInstantiationAux : dvStrings) {
                codes.add(getCodeFromDVInstantiation(dvInstantiationAux));
            }
            return codes;
        }else{
            return null;
        }
    }

    public static String roundToStr(double unrounded, int precision){
        StringBuffer roundSB= new StringBuffer();
        roundSB.append(unrounded);
        String origStr = roundSB.toString();
        if (origStr.contains(".")){
            int numDecimals = origStr.length()-origStr.indexOf(".")-1;
            if (numDecimals!=precision){
                if (numDecimals>precision){
                    if (precision>0){
                        roundSB = new StringBuffer(origStr.substring(0, origStr.indexOf(".")+precision+1));
                    }else{
                        roundSB = new StringBuffer(origStr.substring(0, origStr.indexOf(".")));
                    }
                }else {
                    appendZeros(roundSB, precision-numDecimals);
                }
            }
        }else{
            if (precision>0){
                roundSB.append(".");
                appendZeros(roundSB, precision);
            }
        }
        return roundSB.toString();
    }

    private static void appendZeros(StringBuffer roundSB, int precision){
        for (int i =0; i<precision;i++) {
            roundSB.append("0");
        }
    }

    public static String getDVClassName(String rmName){
        return getDummyDV(rmName).getClass().getSimpleName();
    }

    public static DataValue getDummyDV(String rmName){
        DataValue dv = dataValueMap.get(rmName);
        if(dv == null) {
            throw new IllegalArgumentException("unsupported RM class[" + rmName + "]");
        }else{
            return dv;
        }
    }

    public static String getReadableValue(DataValue dv, TermDefinition termDefinition){
        if (dv instanceof DvCodedText){
            CodePhrase cp = ((DvCodedText)dv).getDefiningCode();
            Term term = getTerm(termDefinition, cp);
            if (term!=null){
                return term.getText();
            }
            return ((DvCodedText)dv).getValue();
        }else if (dv instanceof DvOrdinal){
            CodePhrase cp = ((DvOrdinal)dv).getSymbol().getDefiningCode();
            Term term = getTerm(termDefinition, cp);
            if (term!=null){
                return term.getText();
            }else{
                return ((DvOrdinal)dv).getSymbol().getValue();
            }
        }else if (dv instanceof DvQuantity){
            DvQuantity dvQuantity = ((DvQuantity)dv);
            DecimalFormat format = getDecimalFormat(dvQuantity.getPrecision());
            return format.format(dvQuantity.getMagnitude())+" "+dvQuantity.getUnits();
        }else if (dv instanceof DvProportion){
            DvProportion dvProportion = ((DvProportion)dv);
            DecimalFormat format = getDecimalFormat(dvProportion.getPrecision());
            return format.format(dvProportion.getNumerator())+"/"+format.format(dvProportion.getDenominator());
        }else if (dv instanceof DvDateTime){
            DateFormat df = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, Locale.getDefault());
            Date date = ((DvDateTime)dv).getDateTime().toDate();
            return df.format(date);
        }else if (dv instanceof DvDate){
            DateFormat df = DateFormat.getDateInstance(DateFormat.SHORT);
            Date date = ((DvDate)dv).getDateTime().toDate();
            return df.format(date);
        }else if (dv instanceof DvTime){
            DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
            Date date = ((DvTime)dv).getDateTime().toDate();
            return df.format(date);
        }else if (dv instanceof DvText){
            return "\""+dv.toString()+"\"";
        }else if (dv!=null){
            return dv.toString();
        }else{
            return null;
        }
    }

    private static Term getTerm( TermDefinition termDefinition, CodePhrase cp){
        if (cp.getTerminologyId().getValue().equals(OpenEHRConst.LOCAL)){
            if (termDefinition!=null){
                return termDefinition.getTerms().get(cp.getCodeString());
            }
        }
        return null;
    }

    private static DecimalFormat getDecimalFormat(int precision){
        return OpenEHRNumberFormat.getDecimalFormat(precision);
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