package se.cambio.cds.util;

import org.apache.log4j.Logger;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstanceBuilder;
import se.cambio.cds.model.facade.execution.vo.RuleReference;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class ElementInstanceCollection {

    public final static String EMPTY_CODE = "*EMPTY*";
    private Map<String, Map<String, Map<String, Set<ArchetypeReference>>>> _archetypeReferenceMap = null;

    public void add(ElementInstance elementInstance) {
        add(elementInstance.getArchetypeReference(), null, null);
    }

    public void addAll(Collection<ElementInstance> elementInstances) {
        Set<ArchetypeReference> archetypeReferences = new HashSet<>();
        for (ElementInstance elementInstance : elementInstances) {
            archetypeReferences.add(elementInstance.getArchetypeReference());
        }
        addAll(archetypeReferences, null);
    }

    public void addAll(Collection<ArchetypeReference> archetypeReferences, GuideManager guideManager) {
        for (ArchetypeReference archetypeReferenceToAdd : archetypeReferences) {
            add(archetypeReferenceToAdd, guideManager, null);
        }
    }

    public void add(ArchetypeReference archetypeReferenceToAdd) {
        add(archetypeReferenceToAdd, null, null);
    }

    public void add(ArchetypeReference archetypeReferenceToAdd, GuideManager guideManager, Calendar date) {
        if (archetypeReferenceToAdd instanceof GeneratedArchetypeReference) {
            //Clone the AR
            ArchetypeReference arAux = archetypeReferenceToAdd.clone();
            for (String idElement : archetypeReferenceToAdd.getElementInstancesMap().keySet()) {
                ElementInstance originalEI = archetypeReferenceToAdd.getElementInstancesMap().get(idElement);
                if (originalEI instanceof PredicateGeneratedElementInstance) {
                    PredicateGeneratedElementInstance predicateOriginalEI =
                            (PredicateGeneratedElementInstance) originalEI;
                    //Its a predicate, so keep it as a generated element instance, but resolve the data value
                    DataValue dv = originalEI.getDataValue();
                    if (guideManager != null) {
                        Collection<Guide> guides = getGuides(guideManager, predicateOriginalEI, dv);
                        dv = ElementInstanceCollectionUtil.resolvePredicate(dv, predicateOriginalEI.getOperatorKind(), guides, date);
                        //Might be null i.e. max(date/time) or path!=null
                    }
                    PredicateGeneratedElementInstance pgei =
                            new PredicateGeneratedElementInstanceBuilder()
                                    .setId(predicateOriginalEI.getId())
                                    .setDataValue(dv)
                                    .setArchetypeReference(arAux)
                                    .setOperatorKind(predicateOriginalEI.getOperatorKind())
                                    .createPredicateGeneratedElementInstance();
                    pgei.setRuleReferences(predicateOriginalEI.getRuleReferences());
                    //Should not allow value changes for generated elements in the future
                } else {
                    //Clone a new instance
                    /*
                    GeneratedElementInstance gei = new GeneratedElementInstance(
                            originalEI.getId(),
                            null, arAux,
                            null, null);
                            */
                    ElementInstance ei = originalEI.clone();
                    ei.setArchetypeReference(arAux);
                    if (originalEI instanceof GeneratedElementInstance) {
                        GeneratedElementInstance generatedElementInstance = (GeneratedElementInstance) originalEI;
                        ((GeneratedElementInstance) ei).setRuleReferences(generatedElementInstance.getRuleReferences());
                    }
                }
            }
            archetypeReferenceToAdd = arAux;
        }


        Set<ArchetypeReference> archetypeReferencesInCollection = getArchetypeReferences(archetypeReferenceToAdd);
        archetypeReferencesInCollection.add(archetypeReferenceToAdd);
    }

    private Collection<Guide> getGuides(GuideManager guideManager, GeneratedElementInstance predicateOriginalEI, DataValue dv) {
        Collection<Guide> guides = new ArrayList<>();
        Set<String> guideIds = getReferenceGuideIds(dv, predicateOriginalEI.getRuleReferences());
        for (String guideId : guideIds) {
            Guide guide = guideManager.getGuide(guideId);
            if (guide == null) {
                Logger.getLogger(ElementInstanceCollectionUtil.class).warn("Guideline not found resolving rule reference '" + guideId + "'");
            } else {
                guides.add(guide);
            }
        }
        return guides;
    }

    public Set<String> getReferenceGuideIds(DataValue dv, Collection<RuleReference> ruleReferences) {
        Set<String> guideIds = new HashSet<>();
        if (dv instanceof DvCodedText) {
            DvCodedText dvCT = (DvCodedText) dv;
            for (RuleReference ruleReference : ruleReferences) {
                if (ruleReference.getGTCode().equals(dvCT.getCode())) {
                    guideIds.add(ruleReference.getGuideId());
                }
            }
        } else {
            for (RuleReference ruleReference : ruleReferences) {
                guideIds.add(ruleReference.getGuideId());
            }
        }
        return guideIds;
    }

    public void remove(ArchetypeReference archetypeReference) {
        getArchetypeReferences(archetypeReference).remove(archetypeReference);
    }

    public void removeDomain(String idDomain) {
        for (String idArchetype : getArchetypeReferenceMap().keySet()) {
            getArchetypeReferenceMap(idArchetype, idDomain).clear();
        }
    }

    public boolean matches(GeneratedArchetypeReference generatedArchetypeReference, Map<String, Guide> guideMap, Calendar date) {
        boolean matches = false;
        Iterator<ArchetypeReference> i = getArchetypeReferences(generatedArchetypeReference).iterator();
        while (i.hasNext() && !matches) {
            ArchetypeReference ar = i.next();
            matches = ElementInstanceCollectionUtil.matchAndFill(generatedArchetypeReference, ar, guideMap, date);
        }
        return matches;
    }

    public Set<String> getElementIdsByIdDomain(String idDomain) {
        Set<String> idElements = new HashSet<>();
        Set<ElementInstance> elementInstances = getAllElementInstancesByDomain(idDomain);
        for (ElementInstance elementInstance : elementInstances) {
            idElements.add(elementInstance.getId());
        }
        return idElements;
    }

    public Collection<ElementInstance> getAllElementInstances() {
        Collection<ElementInstance> elementInstances = new ArrayList<>();
        for (ArchetypeReference ar : getAllArchetypeReferences()) {
            elementInstances.addAll(ar.getElementInstancesMap().values());
        }
        return elementInstances;
    }

    public Set<ArchetypeReference> getAllArchetypeReferences() {
        Set<ArchetypeReference> archetypeReferences = new HashSet<>();
        for (String idArchetype : getArchetypeReferenceMap().keySet()) {
            archetypeReferences.addAll(getAllArchetypeReferences(idArchetype));
        }
        return archetypeReferences;
    }

    public Set<ElementInstance> getAllElementInstancesByDomain(String idDomain) {
        Set<ElementInstance> elementInstances = new HashSet<>();
        for (ArchetypeReference archetypeReference : getAllArchetypeReferencesByDomain(idDomain)) {
            elementInstances.addAll(archetypeReference.getElementInstancesMap().values());
        }
        return elementInstances;
    }

    public void merge(Collection<ElementInstanceCollection> elementInstanceCollections) {
        for (ElementInstanceCollection eic : elementInstanceCollections) {
            this.merge(eic);
        }
    }

    public void merge(ElementInstanceCollection eic) {
        for (ArchetypeReference archetypeReference : eic.getAllArchetypeReferences()) {
            add(archetypeReference);
        }
    }

    public Set<ArchetypeReference> getArchetypeReferences(ArchetypeReference archetypeReference) {
        String idDomain = archetypeReference.getIdDomain() != null ? archetypeReference.getIdDomain() : EMPTY_CODE;
        String idAux = null;
        if (Domains.CDS_ID.equals(idDomain)) {
            idAux = archetypeReference.getIdTemplate();
        }
        if (idAux == null) {
            idAux = EMPTY_CODE;
        }
        return getArchetypeReferences(archetypeReference.getIdArchetype(), idDomain, idAux);
    }

    public Set<ArchetypeReference> getArchetypeReferencesEmptyAsWildcard(ArchetypeReference ar) {
        String idDomain = ar.getIdDomain();
        String idAux = null;
        if (Domains.CDS_ID.equals(idDomain)) {
            idAux = ar.getIdTemplate();
        }

        if (idDomain == null) {
            Set<ArchetypeReference> arSet = new HashSet<>();
            arSet.addAll(getArchetypeReferencesEmptyAsWildcard(ar.getIdArchetype(), Domains.CDS_ID, idAux));
            arSet.addAll(getArchetypeReferencesEmptyAsWildcard(ar.getIdArchetype(), Domains.EHR_ID, idAux));
            return arSet;
        } else {
            return getArchetypeReferencesEmptyAsWildcard(ar.getIdArchetype(), idDomain, idAux);
        }
    }


    private Set<ArchetypeReference> getArchetypeReferencesEmptyAsWildcard(String idArchetype, String idDomain, String idAux) {
        Set<ArchetypeReference> arSet = new HashSet<>();
        if (idAux == null) {
            for (Set<ArchetypeReference> arSetAux : getArchetypeReferenceMap(idArchetype, idDomain).values()) {
                arSet.addAll(arSetAux);
            }
        } else {
            arSet.addAll(getArchetypeReferences(idArchetype, idDomain, idAux));
        }
        return arSet;
    }


    private Set<ArchetypeReference> getAllArchetypeReferences(String idArchetype) {
        Set<ArchetypeReference> archetypeReferences = new HashSet<>();
        for (String idDomain : getArchetypeReferenceMap(idArchetype).keySet()) {
            archetypeReferences.addAll(getAllArchetypeReferences(idArchetype, idDomain));
        }
        return archetypeReferences;
    }

    private Set<ArchetypeReference> getAllArchetypeReferences(String idArchetype, String idDomain) {
        Set<ArchetypeReference> archetypeReferences = new HashSet<>();
        Map<String, Set<ArchetypeReference>> archetypeReferencesMap = getArchetypeReferenceMap(idArchetype, idDomain);
        for (Set<ArchetypeReference> idAux : archetypeReferencesMap.values()) {
            archetypeReferences.addAll(idAux);
        }
        return archetypeReferences;
    }

    private Set<ArchetypeReference> getArchetypeReferences(String idArchetype, String idDomain, String idAux) {
        if (idAux == null) {
            Logger.getLogger(ElementInstanceCollection.class).warn("Call to getArchetypeReferences with idAux=='null'");
        }
        Set<ArchetypeReference> archetypeReferences =
                getArchetypeReferenceMap(idArchetype, idDomain).get(idAux);
        if (archetypeReferences == null) {
            archetypeReferences = new HashSet<>();
            getArchetypeReferenceMap(idArchetype, idDomain).put(idAux, archetypeReferences);
        }
        return archetypeReferences;
    }

    private Map<String, Set<ArchetypeReference>> getArchetypeReferenceMap(String idArchetype, String idDomain) {
        if (idDomain == null) {
            Logger.getLogger(ElementInstanceCollection.class).warn("Call to getArchetypeReferenceMap with idDomain=='null'");
        }
        Map<String, Set<ArchetypeReference>> archetypeReferenceMap =
                getArchetypeReferenceMap(idArchetype).get(idDomain);
        if (archetypeReferenceMap == null) {
            archetypeReferenceMap = new HashMap<>();
            getArchetypeReferenceMap(idArchetype).put(idDomain, archetypeReferenceMap);
        }
        return archetypeReferenceMap;
    }

    private Map<String, Map<String, Set<ArchetypeReference>>> getArchetypeReferenceMap(String idArchetype) {
        Map<String, Map<String, Set<ArchetypeReference>>> archetypeReferenceMap =
                getArchetypeReferenceMap().get(idArchetype);
        if (archetypeReferenceMap == null) {
            archetypeReferenceMap = new HashMap<>();
            getArchetypeReferenceMap().put(idArchetype, archetypeReferenceMap);
        }
        return archetypeReferenceMap;
    }

    public Collection<ArchetypeReference> getAllArchetypeReferencesByDomain(String domainId) {
        Collection<ArchetypeReference> ars = new ArrayList<>();
        for (String idArchetype : getArchetypeReferenceMap().keySet()) {
            ars.addAll(getAllArchetypeReferences(idArchetype, domainId));
        }
        return ars;
    }

    private Map<String, Map<String, Map<String, Set<ArchetypeReference>>>> getArchetypeReferenceMap() {
        if (_archetypeReferenceMap == null) {
            _archetypeReferenceMap = new HashMap<>();
        }
        return _archetypeReferenceMap;
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("-----\n");
        for (String idArchetype : getArchetypeReferenceMap().keySet()) {
            sb.append(idArchetype + "\n");
            for (String idDomain : getArchetypeReferenceMap(idArchetype).keySet()) {
                sb.append("-Domain=" + idDomain + "\n");
                for (String idAux : getArchetypeReferenceMap(idArchetype, idDomain).keySet()) {
                    sb.append("--idAux=" + idAux + "\n");
                    int i = 1;
                    for (ArchetypeReference ar : getArchetypeReferences(idArchetype, idDomain, idAux)) {
                        sb.append("---[" + i + "]\n");
                        for (String idElement : ar.getElementInstancesMap().keySet()) {
                            sb.append("----" + idElement + "");
                            ElementInstance ei = ar.getElementInstancesMap().get(idElement);
                            if (ei.getDataValue() != null) {
                                sb.append(" (" + ei.getDataValue().toString() + ")");
                            }
                            sb.append("\n");
                        }
                    }
                }
            }
        }
        sb.append("-----\n");
        return sb.toString();
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */