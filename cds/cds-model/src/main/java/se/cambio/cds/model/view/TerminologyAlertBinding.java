package se.cambio.cds.model.view;

import org.openehr.rm.datatypes.text.DvCodedText;

import java.util.HashMap;
import java.util.Map;

/**
 * User: iago.corbal
 * Date: 2014-09-19
 * Time: 15:47
 */
public class TerminologyAlertBinding {
    private String terminologyId;
    private Map<String, DvCodedText> terminologyAlertBindings;

    public TerminologyAlertBinding() {
    }

    public TerminologyAlertBinding(String terminologyId, Map<String, DvCodedText> terminologyAlertBindings) {
        this.terminologyId = terminologyId;
        this.terminologyAlertBindings = terminologyAlertBindings;
    }

    public String getTerminologyId() {
        return terminologyId;
    }

    public void setTerminologyId(String terminologyId) {
        this.terminologyId = terminologyId;
    }

    public Map<String, DvCodedText> getTerminologyAlertBindings() {
        if (terminologyAlertBindings == null) {
            terminologyAlertBindings = new HashMap<String, DvCodedText>();
        }
        return terminologyAlertBindings;
    }

    public void setTerminologyAlertBindings(Map<String, DvCodedText> terminologyAlertBindings) {
        this.terminologyAlertBindings = terminologyAlertBindings;
    }
}
