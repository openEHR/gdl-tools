package se.cambio.cds.gdl.model.expression;

import junit.framework.TestCase;
import org.openehr.rm.datatypes.quantity.DvOrdinal;
import org.openehr.rm.datatypes.quantity.DvQuantity;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;

public class ImmutableConstantObjectsTest extends TestCase {

    public void test_quantity_constant_is_immutable() {
        QuantityConstant quantityConstant = new QuantityConstant(new DvQuantity("d", 0.0, 0));
        DvQuantity dvQuantity = quantityConstant.getQuantity();
        dvQuantity.setMagnitude(8.0);
        assertEquals(0.0, quantityConstant.getQuantity().getMagnitude());
    }

    public void test_coded_text_constant_is_immutable() {
        CodedTextConstant codedTextConstant = new CodedTextConstant("Diabetes", new CodePhrase("ICD10", "E11"));
        DvCodedText dvCodedText = codedTextConstant.getCodedText();
        dvCodedText.setDefiningCode(new CodePhrase("ICD10", "I48"));
        assertEquals("E11", codedTextConstant.getCodedText().getDefiningCode().getCodeString());
    }

    public void test_code_phrase_constant_is_immutable() {
        CodePhraseConstant codePhraseConstant = new CodePhraseConstant(new CodePhrase("ICD10", "E11"));
        CodePhrase codePhrase = codePhraseConstant.getCodePhrase();
        codePhrase.setCodeString("I48");
        assertEquals("E11", codePhraseConstant.getCodePhrase().getCodeString());
    }

    public void test_ordinal_constant_is_immutable() {
        OrdinalConstant ordinalConstant = new OrdinalConstant(new DvOrdinal(1, new DvCodedText("Good", "local", "gt0001")));
        DvOrdinal dvOrdinal = ordinalConstant.getOrdinal();
        dvOrdinal.setValue(0);
        dvOrdinal.setSymbol(new DvCodedText("Bad", "global", "1233455"));
        assertEquals(1, ordinalConstant.getOrdinal().getValue());
        assertEquals("Good", ordinalConstant.getOrdinal().getSymbol().getValue());
        assertEquals("local", ordinalConstant.getOrdinal().getTerminologyId());
        assertEquals("gt0001", ordinalConstant.getOrdinal().getSymbol().getCode());
    }

}
