package se.cambio.cds.controller.session.data;

import se.cambio.cm.model.util.CMType;
import se.cambio.openehr.controller.session.data.*;
import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class CMManager implements CMManagerI<CMElement> {

    private static CMManager instance;
    Map<CMType, AbstractCMManager<? extends CMElement>> cmElementManagerMap;

    private CMManager() {
        getCmElementManagerMap().put(CMType.TERMINOLOGY, new Terminologies());
        ArchetypeManager archetypeManager = new ArchetypeManager();
        getCmElementManagerMap().put(CMType.ARCHETYPE, new Archetypes(archetypeManager));
        getCmElementManagerMap().put(CMType.TEMPLATE, new Templates(archetypeManager));
        getCmElementManagerMap().put(CMType.GUIDELINE, new Guides());
        getCmElementManagerMap().put(CMType.VIEW, new DecisionSupportViews());
        getCmElementManagerMap().put(CMType.APP, new CDSApps());
        getCmElementManagerMap().put(CMType.ORDERSET, new OrderSets());
        getCmElementManagerMap().put(CMType.INSTANCE, new KBInstances());
    }

    @Override
    public Collection<CMElement> getAllInCache() {
        return null; //Generated
    }

    @Override
    public Collection<String> getAllIds() throws InternalErrorException {
        return null; //Generated
    }

    @Override
    public Collection<String> getAllIdsInCache() throws InternalErrorException {
        return null; //Generated
    }

    @Override
    public CMElement getCMElement(String id) throws InstanceNotFoundException, InternalErrorException {
        return null; //Generated
    }

    @Override
    public Collection<CMElement> getCMElementsInCache(Collection<String> ids) throws InstanceNotFoundException, InternalErrorException {
        return null; //Generated
    }

    @Override
    public CMElement getCMElementInCache(String id) throws InstanceNotFoundException, InternalErrorException {
        return null; //Generated
    }

    @Override
    public Collection<CMElement> getCMElementByIds(Collection<String> ids) throws InstanceNotFoundException, InternalErrorException {
        return null; //Generated
    }

    @Override
    public void remove(String id) throws InstanceNotFoundException, InternalErrorException {
        //Generated
    }

    @Override
    public void upsert(CMElement cmElement) throws InternalErrorException {
        //Generated
    }

    @Override
    public Date getLastUpdate() throws InternalErrorException {
        return null; //Generated
    }

    @Override
    public String getCachedChecksum() throws InternalErrorException {
        return null; //Generated
    }

    public Map<CMType, AbstractCMManager<? extends CMElement>> getCmElementManagerMap() {
        if (cmElementManagerMap == null) {
            cmElementManagerMap = new HashMap<CMType, AbstractCMManager<? extends CMElement>>();
        }
        return cmElementManagerMap;
    }

    public static CMManager getInstance() {
        if (instance == null) {
            instance = new CMManager();
        }
        return instance;
    }
}
