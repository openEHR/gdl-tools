package se.cambio.cm.model.util;

public enum CMTypeFormat {
    ADL_FORMAT("adl"),
    ADLS_FORMAT("adls"),
    OET_FORMAT("oet"),
    CSV_FORMAT("csv"),
    GDL_FORMAT("gdl");

    private final String format;

    CMTypeFormat(String format){
        this.format = format;
    }

    public String getFormat() {
        return format;
    }
}
