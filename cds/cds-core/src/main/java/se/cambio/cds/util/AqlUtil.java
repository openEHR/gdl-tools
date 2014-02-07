package se.cambio.cds.util;

import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.exceptions.AQLGenerationException;

import java.util.*;

public class AqlUtil{

    public static String getAql(Collection<String> ehrIds, ArchetypeReference archetypeReference)
            throws AQLGenerationException {

        final StringBuilder sb = new StringBuilder();

        LinkedHashSet<String> elementPaths = new LinkedHashSet<String>();
        List<String> elementIds = new ArrayList<String>(archetypeReference.getElementInstancesMap().keySet());
        Collections.sort(elementIds);
        for (String elementId : elementIds){
            String elementPath = elementId.substring(archetypeReference.getIdArchetype().length());
            if (elementPath.equals("/event/time")){
                elementPath = "/data/events/time"; //TODO!!
            }
            elementPaths.add(elementPath);
        }

        sb.append("SELECT\n");
        sb.append("e/ehr_id/value AS ehrId\n");
            for (String elementPath : elementPaths){
                sb.append(", a").append(elementPath).append('\n');
            }
        sb.append("FROM\n");
        sb.append("EHR e CONTAINS (\n");
        sb.append("COMPOSITION c").append(" CONTAINS ").append(getKind(archetypeReference.getIdArchetype())).append(" a").append(" [").append(archetypeReference.getIdArchetype()).append("]\n");
        sb.append(")\n");
        if (ehrIds!=null && !ehrIds.isEmpty()){
            sb.append("WHERE e/ehr_id/value matches {\n");
            final Iterator<String> ehrIdsIterator = ehrIds.iterator();
            sb.append('\'').append(ehrIdsIterator.next()).append("\'\n");
            while (ehrIdsIterator.hasNext()){
                sb.append(",\'").append(ehrIdsIterator.next()).append("\'\n");
            }
            sb.append("}\n");
        }else{
            sb.append("OFFSET 0 FETCH 200\n");
        }
        return sb.toString();
    }

    public static List<ArchetypeReference> compactArchetypeReferences(Collection <ArchetypeReference> ars){
        Map<String, Collection<ArchetypeReference>> arsMap = new HashMap<String, Collection<ArchetypeReference>>();
        for (ArchetypeReference ar : ars){
            Collection<ArchetypeReference> arsAux = arsMap.get(ar.getIdArchetype());
            if (arsAux==null){
                arsAux = new ArrayList<ArchetypeReference>();
                arsMap.put(ar.getIdArchetype(), arsAux);
            }
            arsAux.add(ar);
        }

        List<ArchetypeReference > compactARs = new ArrayList<ArchetypeReference>();
        for (String archetypeId: arsMap.keySet()){
            ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, archetypeId, null);
            for (ArchetypeReference arAux: arsMap.get(archetypeId)){
                for (String elementId: arAux.getElementInstancesMap().keySet()){
                    if (!ar.getElementInstancesMap().containsKey(elementId)){
                        new ElementInstance(elementId, null, ar, null, GuideUtil.NULL_FLAVOUR_CODE_NO_INFO); //The reference to AR is done inside EI initialization
                    }
                }
            }
            compactARs.add(ar);
        }
        return compactARs;
    }

    public static String getKind(final String archetypeId){
        final int i = archetypeId.indexOf('.');
        final int j = archetypeId.substring(0,i).lastIndexOf('-');
        if (j+1<i){
            return archetypeId.substring(j+1,i);
        }
        return "OBSERVATION";  //TODO
    }
}
