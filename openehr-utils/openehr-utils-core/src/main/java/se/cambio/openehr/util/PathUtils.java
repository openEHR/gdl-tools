package se.cambio.openehr.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * User: iago.corbal
 * Date: 2014-03-19
 * Time: 16:11
 */
public class PathUtils {

    public static List<String> getArchetypeIdsInPath(String path, Collection<String> validArcehtypeIds){
        List<String> archetypeIds = new ArrayList<String>();
        String[] elemetnsInPath = path.split("/");
        for(String elementInPath: elemetnsInPath){
            if (elementInPath.contains("[") && elementInPath.contains("]")){
                String candidateArchetypeId = elementInPath.substring(elementInPath.indexOf("[")+1, elementInPath.indexOf("]"));
                if (validArcehtypeIds.contains(candidateArchetypeId)){
                    archetypeIds.add(candidateArchetypeId);
                }
            }
        }
        return archetypeIds;
    }

    public static String getEntryArchetypeInPath(String path){
        String[] elemetnsInPath = path.split("/");
        for(String elementInPath: elemetnsInPath){
            if (elementInPath.contains("[") && elementInPath.contains("]")){
                String candidateArchetypeId = elementInPath.substring(elementInPath.indexOf("[")+1, elementInPath.indexOf("]"));
                if (candidateArchetypeId.contains("-" + OpenEHRConst.ACTION + ".") ||  //TODO Is this reliable??
                        candidateArchetypeId.contains("-" + OpenEHRConst.INSTRUCTION + ".") ||
                        candidateArchetypeId.contains("-" + OpenEHRConst.OBSERVATION + ".") ||
                        candidateArchetypeId.contains("-" + OpenEHRConst.EVALUATION + ".")){
                    return candidateArchetypeId;
                }
            }
        }
        return null;
    }

    public static String getLastArchetypeIdInPath(String path, Collection<String> validArcehtypeIds){
        List<String> archetypeIds = getArchetypeIdsInPath(path, validArcehtypeIds);
        if (!archetypeIds.isEmpty()){
            return archetypeIds.get(0);
        }else{
            return null;
        }
    }
}
