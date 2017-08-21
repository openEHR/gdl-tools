package se.cambio.cds.util;

import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import se.cambio.cds.gdl.model.expression.ExpressionItem;

public class CurrentTimeExpressionDataValue extends DvDateTime {

    private ExpressionItem _expressionItem;
    private String _attribute;

    public CurrentTimeExpressionDataValue(ExpressionItem expressionItem, String attribute) {
        super();
        _expressionItem = expressionItem;
        _attribute = attribute;
    }

    public ExpressionItem getExpressionItem() {
        return _expressionItem;
    }

    public void setExpressionItem(ExpressionItem expressionItem) {
        _expressionItem = expressionItem;
    }

    public String getAttribute() {
        return _attribute;
    }

    public void setAttrbute(String attribute) {
        this._attribute = attribute;
    }
}
