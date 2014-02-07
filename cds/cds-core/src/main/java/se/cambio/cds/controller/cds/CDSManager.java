package se.cambio.cds.controller.cds;

import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.model.facade.execution.vo.GeneratedArchetypeReference;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.facade.execution.vo.PredicateGeneratedElementInstance;
import se.cambio.cds.model.facade.kb.delegate.KBFacadeDelegate;
import se.cambio.cds.model.facade.kb.delegate.KBFacadeDelegateFactory;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.DVUtil;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cds.util.GeneratedElementInstanceCollection;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.util.*;

public class CDSManager {

    public static Collection<ElementInstance> getElementInstances(
            String ehrId,
            Collection<String> guideIds,
            Collection<ArchetypeReference> ehrData,
            GuideManager guideManager,
            Calendar date)
            throws PatientNotFoundException, InternalErrorException{
        ElementInstanceCollection eic = new ElementInstanceCollection();
        if (ehrData!=null){
            eic.addAll(ehrData, guideManager);
        }
        GeneratedElementInstanceCollection completeEIC = guideManager.getElementInstanceCollection(guideIds);
        //Search for EHR elements
        //Query EHR for elements
        if (ehrId!=null){
            Collection<ArchetypeReference> ars = getEHRArchetypeReferences(completeEIC);
            Map<String, Collection<ElementInstance>> elementInstanceMap =
                    CDSSessionManager.getEHRFacadeDelegate().queryEHRElements(Collections.singleton(ehrId), ars, date);
            Collection<ElementInstance> elementInstances = elementInstanceMap.get(ehrId);
            if (elementInstances!=null){
                Set<ArchetypeReference> archetypeReferences = new HashSet<ArchetypeReference>();
                for (ElementInstance elementInstance: elementInstances){
                    archetypeReferences.add(elementInstance.getArchetypeReference());
                }
                eic.addAll(archetypeReferences, null);
            }
        }
        return getElementInstances(eic, completeEIC, guideManager, date);
    }

    public static Map<String,Collection<ElementInstance>> getElementInstancesForPopulation(
            Collection <String> ehrIds,
            Collection<String> guideIds,
            Collection<ArchetypeReference> ehrData,
            GuideManager guideManager,
            Calendar date) throws PatientNotFoundException, InternalErrorException{
        GeneratedElementInstanceCollection completeEIC = guideManager.getElementInstanceCollection(guideIds);
        Collection<ArchetypeReference> ars = getEHRArchetypeReferences(completeEIC);
        Map<String,Collection<ElementInstance>> ehrMap =
                CDSSessionManager.getEHRFacadeDelegate().queryEHRElements(ehrIds, ars, date);
        Map<String,Collection<ElementInstance>> cdsEIMap = new HashMap<String, Collection<ElementInstance>>();
        for (String ehrId : ehrIds) {
            ElementInstanceCollection eic = new ElementInstanceCollection();
            //TODO If the data existed in ehrData, it should not query for it again to EHR
            if (!ars.isEmpty()){
                Collection<ElementInstance> eis = ehrMap.get(ehrId);
                if (eis!=null){
                    eic.addAll(eis);
                }
            }
            cdsEIMap.put(ehrId, getElementInstances(eic, completeEIC, guideManager, date));
        }
        return cdsEIMap;
    }


    public static Collection<ArchetypeReference> getEHRArchetypeReferences(GeneratedElementInstanceCollection eic){
        Collection<ArchetypeReference> ars = new ArrayList<ArchetypeReference>();
        ars.addAll(eic.getAllArchetypeReferencesByDomain(Domains.EHR_ID));
        ars.addAll(eic.getAllArchetypeReferencesByDomain(ElementInstanceCollection.EMPTY_CODE));
        return getCompressedQueryArchetypeReferences(ars);
    }

    private static Collection<ElementInstance> getElementInstances(ElementInstanceCollection eic, GeneratedElementInstanceCollection completeEIC, GuideManager guideManager, Calendar date)
            throws InternalErrorException{

        KBFacadeDelegate kbfd = KBFacadeDelegateFactory.getDelegate();

        //Search for CDS templates (not looking into 'ANY' Domain)
        Collection<ElementInstance> cdsGeneratedElementInstances = guideManager.getCompleteElementInstanceCollection().getAllElementInstancesByDomain(Domains.CDS_ID);
        Set<String> idTemplates = new HashSet<String>();
        for (ElementInstance elementInstance : cdsGeneratedElementInstances) {
            String idTemplate = elementInstance.getArchetypeReference().getIdTemplate();
            if (idTemplate!=null){
                idTemplates.add(idTemplate);
            }
        }
        //Query KB for CDS elements
        if (!idTemplates.isEmpty()){
            eic.addAll(kbfd.getKBElementsByIdTemplate(idTemplates));
        }

        //Check for missing elements
        checkForMissingElements(eic, completeEIC, guideManager, date);
        return eic.getAllElementInstances();
    }

    public static void checkForMissingElements(
            ElementInstanceCollection elementInstanceCollection,
            ElementInstanceCollection completeEIC,
            GuideManager guideManager,
            Calendar date){
        //Check for guide elements, if not present, create archetype reference
        for (ArchetypeReference archetypeReference : completeEIC.getAllArchetypeReferences()) {
            GeneratedArchetypeReference gar = (GeneratedArchetypeReference)archetypeReference;
            Guide referencedGuide = getReferencedGuideInPredicate(gar, guideManager);
            boolean matches = elementInstanceCollection.matches(gar, referencedGuide, date);
            if (!matches){
                elementInstanceCollection.add(archetypeReference, guideManager, date);
            }
        }
    }

    private static Guide getReferencedGuideInPredicate(GeneratedArchetypeReference gar, GuideManager gm){
        Iterator<ElementInstance> i = gar.getElementInstancesMap().values().iterator();
        while(i.hasNext()){
            ElementInstance ei = i.next();
            if (ei instanceof PredicateGeneratedElementInstance){
                String idGuide = ((PredicateGeneratedElementInstance)ei).getGuideId();
                return gm.getGuide(idGuide);
            }
        }
        return null;
    }

    private static Collection<ArchetypeReference> getCompressedQueryArchetypeReferences(Collection<ArchetypeReference> generatedArchetypeReferences){
        Map<String, ArchetypeReference> archetypeReferencesMap = new HashMap<String, ArchetypeReference>();
        //Compress Archetype References with same archetype id
        for (ArchetypeReference arNew : generatedArchetypeReferences) {
            ArchetypeReference arPrev = archetypeReferencesMap.get(arNew.getIdArchetype());
            if (arPrev!=null){
                compressQueryArchetypeReference(arPrev, arNew);
            }else{
                arNew = getCleanArchetypeReferenceWithElements(arNew);
                archetypeReferencesMap.put(arNew.getIdArchetype(), arNew);
            }
        }
        return new ArrayList<ArchetypeReference>(archetypeReferencesMap.values());
    }

    private static void compressQueryArchetypeReference(ArchetypeReference arPrev, ArchetypeReference arNew){
        for (ElementInstance newEI : arNew.getElementInstancesMap().values()) {
            ElementInstance eiAux = arPrev.getElementInstancesMap().get(newEI.getId());
            if (eiAux==null){
                //Missing elements
                cloneElementInstanceWithGTCodes(newEI, arPrev, false);
            }else{
                if (newEI instanceof PredicateGeneratedElementInstance){
                    PredicateGeneratedElementInstance pgeiNew = (PredicateGeneratedElementInstance)newEI;
                    ElementInstance prevEI = arPrev.getElementInstancesMap().get(pgeiNew.getId());
                    if (prevEI instanceof PredicateGeneratedElementInstance){
                        PredicateGeneratedElementInstance pgeiPrev = (PredicateGeneratedElementInstance)prevEI;
                        if (!pgeiNew.getOperatorKind().equals(pgeiPrev.getOperatorKind()) ||
                                DVUtil.compareDVs(pgeiNew.getDataValue(), pgeiPrev.getDataValue())!=0){
                            //TODO Find a predicate (if possible) that includes both
                            //Incompatible predicates found, we remove data value and operation
                            pgeiPrev.setDataValue(null);
                            pgeiPrev.setOperatorKind(null);
                        }
                    }
                }
                if (eiAux instanceof GeneratedElementInstance){
                    //Clear GT Code, archetype is referenced twice in guide
                    GeneratedElementInstance gei = (GeneratedElementInstance) eiAux;
                    gei.setGtCode(null);
                }
            }
        }
    }

    private static ArchetypeReference getCleanArchetypeReferenceWithElements(ArchetypeReference ar){
        ArchetypeReference arNew = ar.clone();
        for (ElementInstance ei : ar.getElementInstancesMap().values()) {
            cloneElementInstanceWithGTCodes(ei, arNew, true);
        }
        return arNew;
    }

    private static ElementInstance cloneElementInstanceWithGTCodes(ElementInstance ei, ArchetypeReference ar, boolean useGTCodes){
        if (useGTCodes && ei instanceof GeneratedElementInstance){
            GeneratedElementInstance gei = (GeneratedElementInstance) ei;
            new GeneratedElementInstance(
                    gei.getId(), null, ar, null, OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO, gei.getGuideId(), gei.getGtCode());
        }else{
            new ElementInstance(ei.getId(), null, ar, null, OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        }
        return ei;
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