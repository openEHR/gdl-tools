package se.cambio.cm.model.archetype.vo;

import java.io.Serializable;

public class ArchetypeTermVO implements Serializable {

    private static final long serialVersionUID = 25042322L;
    private String archetypeId;
    private String code;
    private String language;
    private String text;
    private String description;

    public ArchetypeTermVO(
            String archetypeId,
            String code,
            String language,
            String text,
            String description) {
        this.archetypeId = archetypeId;
        this.code = code;
        this.language = language;
        this.text = text;
        this.description = description;
    }

    public String getArchetypeId() {
        return archetypeId;
    }

    public String getCode() {
        return code;
    }

    public String getLanguage() {
        return language;
    }

    public String getText() {
        return text;
    }

    public String getDescription() {
        return description;
    }
}
