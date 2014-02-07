package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.*;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.quantity.datetime.DvDuration;
import org.openehr.rm.datatypes.quantity.datetime.DvTemporal;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.datatypes.text.DvText;
import org.openehr.rm.support.measurement.SimpleMeasurementService;
import se.cambio.cds.gdl.model.expression.*;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.DataValueGenerator;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.OpenEHRDataValues;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;


public class DVUtil {

    public static DataValue createDV(ElementInstance elementInstance, String rmName, String attributeName, Object value) throws InternalErrorException{
        DataValue dv = elementInstance.getDataValue();
        if(dv==null){
            dv = DataValueGenerator.getDummyDV(rmName);
        }
        return DataValueGenerator.createDV(dv, attributeName, value);
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
            /*
            if (dv2!=null){
                if (ei.getDataValue()!=null){
                    return DVUtil.equalDVs(ei.getDataValue(), dv2);
                }else{
                    return false;
                }
            }else{
                return false;
            } */
            return DVUtil.equalDVs(ei.getDataValue(), dv2);
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
                } catch (InternalErrorException e) {
                    ExceptionHandler.handle(e);
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

    public static boolean isNotSubClassOf(boolean inPredicate, ElementInstance ei, DataValue... dataValues){
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
                } catch (InternalErrorException e) {
                    ExceptionHandler.handle(e);
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

    public static boolean checkMaxMin(DataValue predicateDV, DataValue dv, String opSymbol) throws InternalErrorException{
        if (predicateDV instanceof DvOrdered && dv instanceof DvOrdered){
            int comp = ((DvOrdered) predicateDV).compareTo((DvOrdered)dv);
            if (OperatorKind.MAX.getSymbol().equals(opSymbol)){
                return comp<0;
            }else if (OperatorKind.MIN.getSymbol().equals(opSymbol)){
                return comp>0;
            }else{
                throw new InternalErrorException(new Exception("Operator for predicate '"+opSymbol+"' is not valid."));
            }
        }else{
            return false;
        }
    }

    public static boolean test2(DataValue dv){
        return true;
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