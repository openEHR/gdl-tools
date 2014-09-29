package se.cambio.cds.model.view;

import org.openehr.rm.datatypes.text.DvCodedText;

import java.util.HashMap;
import java.util.Map;

public class TerminologyAlertBinding {
    private String terminologyId;
    private Map<String, DvCodedText> terminologyAlertBindings;

    public TerminologyAlertBinding(String terminologyId) {
        this.terminologyId = terminologyId;
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
}
