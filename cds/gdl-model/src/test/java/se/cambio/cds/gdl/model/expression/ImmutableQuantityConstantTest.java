package se.cambio.cds.gdl.model.expression;

import junit.framework.TestCase;
import org.openehr.rm.datatypes.quantity.DvQuantity;

public class ImmutableQuantityConstantTest extends TestCase {

    public void test_can_quantity_constant_not_be_modified_by_changing_its_returned_object_value() {
        QuantityConstant quantityConstant = new QuantityConstant(new DvQuantity("d", 0.0, 0));
        DvQuantity dvQuantity = quantityConstant.getQuantity();
        dvQuantity.setMagnitude(8.0);
        assertEquals(0.0, quantityConstant.getQuantity().getMagnitude());
    }
}
