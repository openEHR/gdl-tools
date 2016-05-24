package se.cambio.cds.util;

import se.cambio.cds.gdl.model.expression.OperatorKind;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;

import java.io.Serializable;
import java.util.Comparator;

public class PredicateSorter implements Comparator<PredicateGeneratedElementInstance>, Serializable {

    private Integer getComparisonNumber(PredicateGeneratedElementInstance pred) {
        if (OperatorKind.IS_A.equals(pred.getOperatorKind())) {
            return 0;
        } else if (OperatorKind.IS_NOT_A.equals(pred.getOperatorKind())) {
            return 1;
        } else if (OperatorKind.EQUALITY.equals(pred.getOperatorKind())) {
            return 2;
        } else if (OperatorKind.INEQUAL.equals(pred.getOperatorKind())) {
            return 3;
        } else if (OperatorKind.GREATER_THAN_OR_EQUAL.equals(pred.getOperatorKind())) {
            return 4;
        } else if (OperatorKind.LESS_THAN_OR_EQUAL.equals(pred.getOperatorKind())) {
            return 5;
        } else if (OperatorKind.GREATER_THAN.equals(pred.getOperatorKind())) {
            return 6;
        } else if (OperatorKind.LESS_THAN.equals(pred.getOperatorKind())) {
            return 7;
        } else if (OperatorKind.MAX.equals(pred.getOperatorKind())) {
            return 8;
        } else if (OperatorKind.MIN.equals(pred.getOperatorKind())) {
            return 9;
        }
        return 0;
    }

    @Override
    public int compare(PredicateGeneratedElementInstance o1, PredicateGeneratedElementInstance o2) {
        return getComparisonNumber(o1).compareTo(getComparisonNumber(o2));
    }
}
