package se.cambio.cds.model.app;

/**
 * User: iago.corbal
 * Date: 2014-09-23
 * Time: 16:24
 */
public class CDSAppDefinition {
    private String name;
    private String description;

    public CDSAppDefinition() {
    }

    public CDSAppDefinition(String name, String description) {
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
