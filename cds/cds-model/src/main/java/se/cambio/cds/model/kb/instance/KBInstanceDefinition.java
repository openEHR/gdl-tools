package se.cambio.cds.model.kb.instance;

/**
 * User: iago.corbal
 * Date: 2014-09-26
 * Time: 17:23
 */
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
