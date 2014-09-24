package se.cambio.cds.util.export.html.util;

import org.openehr.am.archetype.Archetype;

import java.util.Collection;

/**
 * User: Iago.Corbal
 * Date: 2014-09-15
 * Time: 10:59
 */
public class ArchetypeDefinitionHTMLRenderer {
    private String iconPath = null;

    public ArchetypeDefinitionHTMLRenderer() {
    }

    public String generateHTML(Archetype archetype, String templateId, String lang){
        SimpleArchetypeNode rootNode = SimpleArchetypeNodeUtils.getSimpleArchetypeNode(archetype.getArchetypeId().getValue(), templateId, lang);
        StringBuffer htmlSB = new StringBuffer();
        htmlSB.append("<ul class=\"tree\">");
        htmlSB.append("<li>");
        htmlSB.append(getImageHTMLTag(rootNode));
        htmlSB.append(getNodeHTML(rootNode));
        htmlSB.append("</li>");
        htmlSB.append("</ul>");
        return htmlSB.toString();
    }

    private String getNodeHTML(SimpleArchetypeNode simpleArchetypeNode){
        StringBuffer htmlSB = new StringBuffer();
        htmlSB.append(simpleArchetypeNode.getName());
        Collection<SimpleArchetypeNode> children = simpleArchetypeNode.getChildren();
        if (!children.isEmpty()) {
            htmlSB.append("<ul>");
            for (SimpleArchetypeNode simpleArchetypeNode1 : children) {
                htmlSB.append("<li>");
                htmlSB.append("<div title='"+simpleArchetypeNode1.getDescription()+"'>");
                htmlSB.append(getImageHTMLTag(simpleArchetypeNode1));
                htmlSB.append(" ");
                htmlSB.append(getNodeHTML(simpleArchetypeNode1));
                htmlSB.append("</div>");
                htmlSB.append("</li>");
            }
            htmlSB.append("</ul>");
        }
        return htmlSB.toString();
    }

    private String getImageHTMLTag(SimpleArchetypeNode simpleArchetypeNode){
        String iconFileName = simpleArchetypeNode.getIconFileName();
        StringBuffer htmlSB = new StringBuffer();
        if (iconFileName!=null){
            htmlSB.append("<img src='");
            if (getIconPath()!=null) {
                htmlSB.append(getIconPath());
                if (!getIconPath().endsWith("/")){
                    htmlSB.append("/");
                }
            }
            htmlSB.append(iconFileName);
            htmlSB.append("'/>");
        }
        return htmlSB.toString();
    }

    private static String getBlankSpaces(int length){
        StringBuffer outputBuffer = new StringBuffer(length);
        for (int i = 0; i < length; i++){
            outputBuffer.append("&nbsp;");
        }
        return outputBuffer.toString();
    }

    public String getIconPath() {
        return iconPath;
    }

    public void setIconPath(String iconPath) {
        this.iconPath = iconPath;
    }
}
