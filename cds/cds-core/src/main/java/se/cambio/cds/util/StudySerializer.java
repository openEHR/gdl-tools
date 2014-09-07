package se.cambio.cds.util;

import com.google.gson.Gson;
import se.cambio.cds.model.study.Study;

/**
 * User: Iago.Corbal
 * Date: 2014-09-07
 * Time: 10:51
 */
public class StudySerializer {
    public static Study parseStudy(String studySrc){
        return new Gson().fromJson(studySrc, Study.class);
    }

    public static String serializeStudy(Study study){
        return new Gson().toJson(study);
    }
}
