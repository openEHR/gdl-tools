package se.cambio.cds.util;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;
import se.cambio.cds.model.view.DecisionSupportView;
import se.cambio.cds.model.view.DecisionSupportViewBundle;
import se.cambio.cds.util.export.json.JSONSerialization;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;

public class DSViewParser {

    public static String DECISION_SUPPORT_VIEW_SCRIPT_ID = "decision-support-view";
    private InputStream is;

    public DSViewParser(InputStream is) {
        this.is = is;
    }

    public DecisionSupportViewBundle parseDSView(){
        try {
            InputStreamReader in = new InputStreamReader(is, "UTF-8");
            String dsvSrc = IOUtils.toString(in);
            String jsonSrc = extractJSONSrc(dsvSrc);
            DecisionSupportView decisionSuportView = null;
            if (jsonSrc!=null) {
                decisionSuportView = parseDSView(jsonSrc);
            }else{
                decisionSuportView = ViewPrototyper.getPrototype(UserConfigurationManager.getLanguage());
            }
            return new DecisionSupportViewBundle(decisionSuportView, dsvSrc);
        } catch (UnsupportedEncodingException e) {
            ExceptionHandler.handle(e);
            return null;
        } catch (IOException e) {
            ExceptionHandler.handle(e);
            return null;
        }
    }

    private String extractJSONSrc(final String dsvSrc){
        Document doc = Jsoup.parse(dsvSrc);
        Elements elements = doc.select("script[id="+DECISION_SUPPORT_VIEW_SCRIPT_ID+"]");
        if (!elements.isEmpty()) {
            return elements.get(0).data();
        }else{
            return null;
        }
    }

    private DecisionSupportView parseDSView(final String src){
        try {
            return JSONSerialization.parse(DecisionSupportView.class, src);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
            return null;
        }
    }
}
