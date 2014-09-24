package se.cambio.cds.util.export.html.util;

import java.util.ArrayList;
import java.util.Collection;

/**
 * User: Iago.Corbal
 * Date: 2014-09-17
 * Time: 16:13
 */
public class TerminologyNode {
    private String name;
    private Collection<TerminologyNode> children;

    public TerminologyNode() {
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Collection<TerminologyNode> getChildren() {
        if (children == null) {
            children = new ArrayList<TerminologyNode>();
        }
        return children;
    }

    public void setChildren(Collection<TerminologyNode> children) {
        this.children = children;
    }
}
