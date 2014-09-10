package se.cambio.cds.model.study;

/**
 * User: Iago.Corbal
 * Date: 2014-09-06
 * Time: 11:02
 */
public class StudyDefinition {
    private String name;
    private String description;
    private String inclusionCriteria;
    private String exclusionCriteria;

    public StudyDefinition(String name, String description, String inclusionCriteria, String exclusionCriteria) {
        this.name = name;
        this.description = description;
        this.inclusionCriteria = inclusionCriteria;
        this.exclusionCriteria = exclusionCriteria;
    }

    public StudyDefinition() {
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

    public String getInclusionCriteria() {
        return inclusionCriteria;
    }

    public void setInclusionCriteria(String inclusionCriteria) {
        this.inclusionCriteria = inclusionCriteria;
    }

    public String getExclusionCriteria() {
        return exclusionCriteria;
    }

    public void setExclusionCriteria(String exclusionCriteria) {
        this.exclusionCriteria = exclusionCriteria;
    }
}
