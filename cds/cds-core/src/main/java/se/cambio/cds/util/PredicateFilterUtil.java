package se.cambio.cds.util;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * User: iago.corbal
 * Date: 2013-12-10
 * Time: 09:23
 */
public class PredicateFilterUtil {

    public static void filterByPredicates(
            Collection<ArchetypeReference> definitionArchetypeReferences,
            Collection<ArchetypeReference> ehrArchetypeReferences, Calendar date){
        for (ArchetypeReference archetypeReference: definitionArchetypeReferences){
            for (ElementInstance elementInstance: archetypeReference.getElementInstancesMap().values()){
                if (elementInstance instanceof PredicateGeneratedElementInstance) {
                    PredicateGeneratedElementInstance pgei = (PredicateGeneratedElementInstance) elementInstance;
                    if (OperatorKind.MAX.equals(pgei.getOperatorKind())){
                        filterMaxMin(pgei.getId(), ehrArchetypeReferences, true);
                    }else if (OperatorKind.MIN.equals(pgei.getOperatorKind())){
                        filterMaxMin(pgei.getId(), ehrArchetypeReferences, false);
                    }else if (OperatorKind.GREATER_THAN_OR_EQUAL.equals(pgei.getOperatorKind())){
                        filterGreaterLessThanPredicate(pgei.getId(), pgei.getDataValue(), ehrArchetypeReferences, true, date);
                    }else if (OperatorKind.LESS_THAN_OR_EQUAL.equals(pgei.getOperatorKind())){
                        filterGreaterLessThanPredicate(pgei.getId(), pgei.getDataValue(), ehrArchetypeReferences, false, date);
                    }
                }
            }
        }
    }

    private static void filterMaxMin(String elementId, Collection<ArchetypeReference> ehrArchetypeReferences, boolean max){
        final Set<ArchetypeReference> archetypeReferencesToRemove = new HashSet<ArchetypeReference>();
        ElementInstance maxElementInstance = null;
        for (ArchetypeReference archetypeReference: ehrArchetypeReferences){
            ElementInstance elementInstance = archetypeReference.getElementInstancesMap().get(elementId);
            if (elementInstance!=null){
                if (elementInstance.getDataValue()!=null){
                    if (maxElementInstance==null || isMaxMin(elementInstance, maxElementInstance, max)){
                        if (maxElementInstance!=null){
                            archetypeReferencesToRemove.add(maxElementInstance.getArchetypeReference());
                        }
                        maxElementInstance = elementInstance;
                    }else{
                        archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                    }
                }else{
                    archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                }
            }
        }
        ehrArchetypeReferences.removeAll(archetypeReferencesToRemove);
    }


    private static void filterGreaterLessThanPredicate(
            String elementId, DataValue dv,
            Collection<ArchetypeReference> ehrArchetypeReferences,
            boolean greaterThan, Calendar date){
        if (dv instanceof CurrentTimeExpressionDataValue){
            dv = ElementInstanceCollectionUtil.resolvePredicate(dv, OperatorKind.GREATER_THAN_OR_EQUAL, null, date);
        }
        final Set<ArchetypeReference> archetypeReferencesToRemove = new HashSet<ArchetypeReference>();
        for (ArchetypeReference archetypeReference: ehrArchetypeReferences){
            ElementInstance elementInstance = archetypeReference.getElementInstancesMap().get(elementId);
            if (elementInstance!=null){
                if (elementInstance.getDataValue()!=null){
                    int compare = DVUtil.compareDVs(dv, elementInstance.getDataValue());
                    if (compare!=0 && ((greaterThan && compare>0)||(!greaterThan && compare<0))){
                        archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                    }
                }else{
                    archetypeReferencesToRemove.add(elementInstance.getArchetypeReference());
                }
            }
        }
        ehrArchetypeReferences.removeAll(archetypeReferencesToRemove);
    }

    private static boolean isMaxMin(ElementInstance elementInstance, ElementInstance maxElementInstance, boolean max){
        int compare = DVUtil.compareDVs(elementInstance.getDataValue(), maxElementInstance.getDataValue());
        if (max){
            return compare>0;
        }else{
            return compare<0;
        }
    }

}
