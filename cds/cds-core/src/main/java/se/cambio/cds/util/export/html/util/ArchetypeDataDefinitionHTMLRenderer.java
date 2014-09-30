package se.cambio.cds.util.export.html.util;

import org.openehr.rm.common.archetyped.Locatable;

public class ArchetypeDataDefinitionHTMLRenderer extends ArchetypeDefinitionHTMLRenderer {
    private String iconPath = null;

    public ArchetypeDataDefinitionHTMLRenderer() {
    }

    public String generateHTML(String archetypeId, String templateId, Locatable locatable, String lang){
        SimpleArchetypeNode rootNode = SimpleArchetypeNodeUtils.getSimpleArchetypeNode(archetypeId, templateId, locatable, lang);
        StringBuffer htmlSB = new StringBuffer();
        htmlSB.append("<ul class=\"tree\">");
        htmlSB.append("<li>");
        htmlSB.append(getImageHTMLTag(rootNode));
        htmlSB.append(getNodeHTML(rootNode));
        htmlSB.append("</li>");
        htmlSB.append("</ul>");
        return htmlSB.toString();
    }
}
