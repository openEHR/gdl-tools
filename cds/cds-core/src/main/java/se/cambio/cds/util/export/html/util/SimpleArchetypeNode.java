package se.cambio.cds.util.export.html.util;

import java.util.ArrayList;
import java.util.Collection;

public class SimpleArchetypeNode {
    private String elementId;
    private String name;
    private String description;
    private String iconFileName;
    private Collection<SimpleArchetypeNode> children;

    public SimpleArchetypeNode(String elementId, String name, String description, String iconFileName) {
        this.elementId = elementId;
        this.name = name;
        this.description = description;
        this.iconFileName = iconFileName;
        this.children = new ArrayList<SimpleArchetypeNode>();
    }

    public String getElementId() {
        return elementId;
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

    public String getIconFileName() {
        return iconFileName;
    }

    public Collection<SimpleArchetypeNode> getChildren() {
        return children;
    }
}
