package se.cambio.cds.model.kb.instance;

public class KBInstanceDefinition {
    private String name;
    private String description;

    public KBInstanceDefinition() {
    }

    public KBInstanceDefinition(String name, String description) {
        this.name = name;
        this.description = description;
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
}
