package se.cambio.cds.util.export.json.util;

import se.cambio.cds.model.kb.instance.KBInstance;

public class KBInstanceWithSource extends KBInstance {
    private String locatableSrc;

    public KBInstanceWithSource(String kbiId) {
        super(kbiId);
    }

    public String getLocatableSrc() {
        return locatableSrc;
    }

    public void setLocatableSrc(String locatableSrc) {
        this.locatableSrc = locatableSrc;
    }
}
