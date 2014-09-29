package se.cambio.cds.model.dsvdata;

import java.util.Collection;

public class DSVGTCodeDef {

    private String gtCode;
    private String name;
    private String description;
    private String RMName;
    private Collection<String> selections;
    private DSVGTCodeType type = null;

    public DSVGTCodeDef(String gtCode, String name, String description, String RMName, Collection<String> selections, DSVGTCodeType type) {
        this.gtCode = gtCode;
        this.name = name;
        this.description = description;
        this.RMName = RMName;
        this.selections = selections;
        this.type = type;
    }

    public String getGtCode() {
        return gtCode;
    }

    public void setGtCode(String gtCode) {
        this.gtCode = gtCode;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getRMName() {
        return RMName;
    }

    public void setRMName(String RMName) {
        this.RMName = RMName;
    }

    public Collection<String> getSelections() {
        return selections;
    }

    public void setSelections(Collection<String> selections) {
        this.selections = selections;
    }

    public DSVGTCodeType getType() {
        return type;
    }

    public void setType(DSVGTCodeType type) {
        this.type = type;
    }
}
