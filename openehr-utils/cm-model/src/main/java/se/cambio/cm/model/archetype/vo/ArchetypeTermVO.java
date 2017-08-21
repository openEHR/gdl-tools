package se.cambio.cm.model.archetype.vo;

import lombok.Builder;
import lombok.Data;

import java.io.Serializable;

@Data
@Builder(toBuilder = true)
public class ArchetypeTermVO implements Serializable {

    private static final long serialVersionUID = 25042322L;
    private String archetypeId;
    private String code;
    private String language;
    private String text;
    private String description;
}
