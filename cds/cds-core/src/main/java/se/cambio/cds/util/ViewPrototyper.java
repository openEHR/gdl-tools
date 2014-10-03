package se.cambio.cds.util;

import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.cds.gdl.model.ResourceDescriptionItem;
import se.cambio.cds.model.view.DecisionSupportView;
import se.cambio.cds.model.view.DecisionSupportViewDefinition;

/**
 * User: iago.corbal
 * Date: 2014-09-19
 * Time: 16:02
 */
public class ViewPrototyper {
    private static String LANGUAGE_TERMINOLOGY = "ISO_639-1";

    public static DecisionSupportView getPrototype(String lang){
        DecisionSupportView decisionSupportView = new DecisionSupportView();
        decisionSupportView.setDsViewId("unknown");
        decisionSupportView.getLanguage().setOriginalLanguage(new CodePhrase(LANGUAGE_TERMINOLOGY, lang));
        decisionSupportView.getResourceDescription().getDetails().put(lang, new ResourceDescriptionItem());
        decisionSupportView.getDecisionSupportViewDefinitions().put(lang, new DecisionSupportViewDefinition());
        return decisionSupportView;
    }

    public static String getDSVSrcPrototype(){
        return "<html></html>";
    }
}
