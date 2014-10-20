package se.cambio.cds.util.export.html.util;

import org.openehr.rm.common.archetyped.Locatable;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class ArchetypeDataDefinitionHTMLRenderer extends ArchetypeDefinitionHTMLRenderer {

    public ArchetypeDataDefinitionHTMLRenderer(ArchetypeManager archetypeManager) {
        super(archetypeManager);
    }

    public String generateHTML(String archetypeId, String templateId, Locatable locatable, String lang) throws InstanceNotFoundException, InternalErrorException {
        SimpleArchetypeNode rootNode = SimpleArchetypeNodeUtils.getSimpleArchetypeNode(archetypeId, templateId, locatable, lang, getArchetypeManager());
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
