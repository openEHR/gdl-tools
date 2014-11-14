package se.cambio.cds.gdl.model.readable.util;

import se.cambio.cm.model.archetype.vo.ArchetypeElementVO;

public class PredicateAttributeVO {
    private ArchetypeElementVO archetypeElementVO;
    private String attribute;

    public PredicateAttributeVO(ArchetypeElementVO archetypeElementVO, String attribute) {

        this.archetypeElementVO = archetypeElementVO;
        this.attribute = attribute;
    }

    public ArchetypeElementVO getArchetypeElementVO() {
        return archetypeElementVO;
    }

    public void setArchetypeElementVO(ArchetypeElementVO archetypeElementVO) {
        this.archetypeElementVO = archetypeElementVO;
    }

    public String getAttribute() {
        return attribute;
    }

    public void setAttribute(String attribute) {
        this.attribute = attribute;
    }


}
