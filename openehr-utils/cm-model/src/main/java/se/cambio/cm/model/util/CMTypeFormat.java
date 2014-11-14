package se.cambio.cm.model.util;

public enum CMTypeFormat {
    ADL_FORMAT("adl"),
    ADLS_FORMAT("adls"),
    OET_FORMAT("oet"),
    CSV_FORMAT("csv"),
    GDL_FORMAT("gdl"),
    STD_FORMAT("std"),
    DSV_FORMAT("dsv"),
    KBI_FORMAT("kbi"),
    OST_FORMAT("ost"),
    APP_FORMAT("app"),
    SCN_FORMAT("scn");

    private final String format;

    CMTypeFormat(String format){
        this.format = format;
    }

    public String getFormat() {
        return format;
    }
}
