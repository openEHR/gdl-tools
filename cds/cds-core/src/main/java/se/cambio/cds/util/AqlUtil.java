package se.cambio.cds.util;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

/**
 * @author Jure Grom
 */
public class AqlUtil
{
    public static Map<ArchetypeReference, String> getAQLs(Collection<String> ehrIds, Collection<ArchetypeReference> archetypeReferences)
    {
	Map<ArchetypeReference, String> aqlsMap = new HashMap<ArchetypeReference, String>();
	for (ArchetypeReference archetypeReference : archetypeReferences) {
	    aqlsMap.put(archetypeReference, getAql(ehrIds, Collections.singleton(archetypeReference)));
	}
	return aqlsMap;
    }

    public static String getAql(Collection<String> ehrIds, Collection<ArchetypeReference> archetypeReferences)
    {

	final StringBuilder sb = new StringBuilder();
	sb.append("SELECT\n");
	/* TODO AGGREGATION SHOULD BE DONE HERE
	if (archetypeReferences.size()==1 && AggregationFunctions.ID_AGGREGATION_FUNCTION_LAST.equals(archetypeReferences.iterator().next().getAggregationFunction()))
	{
	    sb.append("TOP 1\n");
	}
	*/
	sb.append("e/ehr_id/value AS ehrId\n");
	int i=0;
	for (ArchetypeReference archetypeReference : archetypeReferences)
	{
	    for (Map.Entry<String, ElementInstance> e : archetypeReference.getElementInstancesMap().entrySet()){
		sb.append(", a").append(i).append(e.getValue().getId().replace(archetypeReference.getIdArchetype(),"")).append('\n');//.append(" AS ").append(i).append("_").append(e.getKey()).append('\n');
	    }
	    i++;
	}
	sb.append("FROM\n");
	sb.append("EHR e CONTAINS (\n");

	final Iterator<ArchetypeReference> archetypeReferenceIterator = archetypeReferences.iterator();
	i=0;
	appendCompositionContainsArchetype(sb,archetypeReferenceIterator.next(),i++);
	while (archetypeReferenceIterator.hasNext())
	{
	    sb.append("AND ");
	    appendCompositionContainsArchetype(sb,archetypeReferenceIterator.next(),i++);
	}

	sb.append(")\n");
	if (ehrIds!=null && !ehrIds.isEmpty())
	{
	    sb.append("WHERE e/ehr_id/value matches {\n");
	    final Iterator<String> ehrIdsIterator = ehrIds.iterator();
	    sb.append('\'').append(ehrIdsIterator.next()).append("\'\n");
	    while (ehrIdsIterator.hasNext())
	    {
		sb.append(",\'").append(ehrIdsIterator.next()).append("\'\n");
	    }
	    sb.append("}\n");
	}
	sb.append("ORDER BY\n");
	for (int j = 0; j < archetypeReferences.size(); j++)
	{
	    if (j!=0)
	    {
		sb.append(',');
	    }
	    sb.append("c").append(j).append("/context/start_time DESC\n");
	}

	if (ehrIds==null || ehrIds.isEmpty())
	{
	    sb.append("OFFSET 0 FETCH 200\n");
	}

	return sb.toString();
    }

    private static void appendCompositionContainsArchetype(final StringBuilder sb, final ArchetypeReference archetypeReference, final int index)
    {
	/* TODO AGGREGATION SHOULD BE DONE HERE
	if (AggregationFunctions.ID_AGGREGATION_FUNCTION_LAST.equals(archetypeReference.getAggregationFunction()))
	{
	    sb.append("TOP 1 ");
	}
	*/
	sb.append("COMPOSITION c").append(index).append(" CONTAINS ").append(getKind(archetypeReference.getIdArchetype())).append(" a").append(index).append(" [").append(archetypeReference.getIdArchetype()).append("]\n");
    }

    private static String getKind(final String archetypeId)
    {
	final int i = archetypeId.indexOf('.');
	final int j = archetypeId.substring(0,i).lastIndexOf('-');
	if (j+1<i)
	{
	    return archetypeId.substring(j+1,i);
	}
	return "OBSERVATION";
    }
}
