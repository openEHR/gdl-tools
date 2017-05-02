package se.cambio.openehr.util;

import java.util.*;

public class PathUtils {

    public static List<String> getArchetypeIdsInPath(String path, Collection<String> validArcehtypeIds) {
        List<String> archetypeIds = new ArrayList<>();
        String[] elementsInPath = path.split("/");
        for (String elementInPath : elementsInPath) {
            if (elementInPath.contains("[") && elementInPath.contains("]")) {
                String candidateArchetypeId = elementInPath.substring(elementInPath.indexOf("[") + 1, elementInPath.indexOf("]"));
                if (validArcehtypeIds.contains(candidateArchetypeId)) {
                    archetypeIds.add(candidateArchetypeId);
                }
            }
        }
        return archetypeIds;
    }

    public static String getLastEntryArchetypeInPath(String path) {
        List<String> entryArchetypes = getEntryArchetypesInPath(path);
        if (entryArchetypes.isEmpty()) {
            return null;
        } else {
            return entryArchetypes.get(entryArchetypes.size() - 1);
        }
    }

    public static List<String> getEntryArchetypesInPath(String path) {
        List<String> entryArchetypes = new ArrayList<>();
        String[] elementsInPath = path.split("/");
        for (String elementInPath : elementsInPath) {
            if (elementInPath.contains("[") && elementInPath.contains("]")) {
                String candidateArchetypeId = elementInPath.substring(elementInPath.indexOf("[") + 1, elementInPath.indexOf("]"));
                if (candidateArchetypeId.contains("-" + OpenEHRConst.ACTION + ".") ||
                        candidateArchetypeId.contains("-" + OpenEHRConst.INSTRUCTION + ".") ||
                        candidateArchetypeId.contains("-" + OpenEHRConst.OBSERVATION + ".") ||
                        candidateArchetypeId.contains("-" + OpenEHRConst.EVALUATION + ".")) {
                    entryArchetypes.add(candidateArchetypeId);
                }
            }
        }
        return entryArchetypes;
    }

    public static String getLastArchetypeIdInPath(String path, Collection<String> validArchetypeIds) {
        List<String> archetypeIds = getArchetypeIdsInPath(path, validArchetypeIds);
        if (!archetypeIds.isEmpty()) {
            return archetypeIds.get(0);
        } else {
            return null;
        }
    }
}
