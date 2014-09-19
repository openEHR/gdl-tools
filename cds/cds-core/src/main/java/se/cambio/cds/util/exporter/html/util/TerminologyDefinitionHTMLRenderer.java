package se.cambio.cds.util.exporter.html.util;

import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.OpenEHRDataValuesUI;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2014-09-15
 * Time: 10:59
 */
public class TerminologyDefinitionHTMLRenderer {
    private String terminologyId;
    private String lang;

    public TerminologyDefinitionHTMLRenderer(String terminologyId, String lang) {
        this.terminologyId = terminologyId;
        this.lang = lang;
    }

    public String generateHTML() throws InternalErrorException {
        Collection<TerminologyNodeVO> terminologyNodeVOs = getTerminologyNodeVOs(terminologyId);
        StringBuffer htmlSB = new StringBuffer();
        htmlSB.append("<ul class=\"tree\">");
        for (TerminologyNodeVO terminologyNodeVOAux : terminologyNodeVOs) {
            htmlSB.append("<li>");
            htmlSB.append(getNodeHTML(terminologyNodeVOAux));
            htmlSB.append("</li>");
        }
        htmlSB.append("</ul>");
        return htmlSB.toString();
    }

    private static String getNodeHTML(TerminologyNodeVO terminologyNodeVO){
        StringBuffer htmlSB = new StringBuffer();
        DvCodedText dvCodedText = terminologyNodeVO.getValue();
        htmlSB.append(dvCodedText.getValue()+" ("+dvCodedText.getCode()+")");
        Collection<TerminologyNodeVO> children = terminologyNodeVO.getChildren();
        if (!children.isEmpty()) {
            htmlSB.append("<ul>");
            for (TerminologyNodeVO terminologyNodeVOAux : children) {
                htmlSB.append("<li>");
                htmlSB.append(getNodeHTML(terminologyNodeVOAux));
                htmlSB.append("</li>");
            }
            htmlSB.append("</ul>");
        }
        return htmlSB.toString();
    }

    public static List<TerminologyNodeVO> getTerminologyNodeVOs(String terminologyId) throws InternalErrorException {
        List<TerminologyNodeVO> nodes =
                OpenEHRSessionManager.getTerminologyFacadeDelegate().retrieveAll(terminologyId, OpenEHRDataValuesUI.getLanguageCodePhrase());
        if (nodes!=null){
            Collections.sort(nodes, new Comparator<TerminologyNodeVO>() {
                @Override
                public int compare(TerminologyNodeVO o1, TerminologyNodeVO o2) {
                    return o1.getValue().getDefiningCode().getCodeString().compareTo(o2.getValue().getDefiningCode().getCodeString());
                }
            });
        }
        return nodes;
    }
}
