package se.cambio.cds.util;

import com.google.gson.Gson;
import se.cambio.cds.model.view.DecisionSupportView;

/**
 * User: Iago.Corbal
 * Date: 2014-09-07
 * Time: 10:51
 */
public class DSViewSerializer {
    public static DecisionSupportView parseDSView(String dsvViewSrc){
        return new Gson().fromJson(dsvViewSrc, DecisionSupportView.class);
    }

    public static String serializeDSView(DecisionSupportView decisionSupportView){
        return new Gson().toJson(decisionSupportView);
    }
}
