package se.cambio.cds.util;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Attribute;
import org.jsoup.nodes.Attributes;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Tag;
import org.jsoup.select.Elements;
import se.cambio.cds.model.view.DecisionSupportView;
import se.cambio.cds.model.view.DecisionSupportViewBundle;
import se.cambio.cds.util.export.json.JSONSerialization;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.HashMap;
import java.util.Map;

public class DSViewSerializer {

    private final DecisionSupportViewBundle decisionSupportViewBundle;

    public DSViewSerializer(DecisionSupportViewBundle decisionSupportViewBundle) {
        this.decisionSupportViewBundle = decisionSupportViewBundle;
    }

    public String serialize(){
        Document doc = Jsoup.parse(decisionSupportViewBundle.getDsvSrc());
        doc.outputSettings().prettyPrint(false);
        Map<Character, String> escapeMapOrig = new HashMap<Character, String>(doc.outputSettings().escapeMode().getMap());
        doc.outputSettings().escapeMode().getMap().clear();
        doc.outputSettings().charset("UTF-8");
        Elements elements = doc.select("script[id=decision-support-view]");
        if (!elements.isEmpty()){
            for(Element element: elements) {
                element.remove();
            }
        }
        Attributes attributes = new Attributes();
        attributes.put(new Attribute("id", DSViewParser.DECISION_SUPPORT_VIEW_SCRIPT_ID));
        attributes.put(new Attribute("type", "application/json"));
        Element element = new Element(Tag.valueOf("script"),"", attributes);
        String jsonSrc = serializeDSView(decisionSupportViewBundle.getDecisionSupportView());
        element.appendText(jsonSrc);
        doc.head().appendChild(element);
        String html = doc.html();
        doc.outputSettings().escapeMode().getMap().putAll(escapeMapOrig);
        return html;
    }

    private String serializeDSView(DecisionSupportView decisionSupportView){
        try {
            return JSONSerialization.serialize(DecisionSupportView.class, decisionSupportView);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
            return null;
        }
    }
}
