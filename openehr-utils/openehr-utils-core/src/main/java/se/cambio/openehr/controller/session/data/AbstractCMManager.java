package se.cambio.openehr.controller.session.data;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.model.util.CMElement;
import se.cambio.cm.model.util.CheckSumManager;
import se.cambio.openehr.util.CachedCMManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.time.Instant;
import java.util.*;

public abstract class AbstractCMManager<E extends CMElement> {

    private Map<String, E> cmElementMap;
    private boolean useCache = true;
    private Logger logger = LoggerFactory.getLogger(AbstractCMManager.class);
    private CachedCMManager cachedCMManager;
    private ClinicalModelsService clinicalModelsService;
    private Instant lastCheckForChanges;

    public AbstractCMManager(ClinicalModelsService clinicalModelsService) {
        this.clinicalModelsService = clinicalModelsService;
        this.cachedCMManager = new CachedCMManager(getCMElementClass(), clinicalModelsService);
        this.lastCheckForChanges = Instant.now();
    }

    public void initialize() {
        getCmElementMap().clear();
    }

    public Collection<E> getAllInCache() {
        return getCmElementMap().values();
    }

    public Collection<String> getAllIds() {
        return clinicalModelsService.getAllCMElementIds(getCMElementClass());
    }

    public Collection<String> getAllIdsInCache() {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot cache, it is not active."));
        }
        return getCmElementMap().keySet();
    }

    public void loadAll() {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot load into cache, it is not active."));
        }
        Collection<E> cmElements = clinicalModelsService.getAllCMElements(getCMElementClass());
        registerCMElementsInCache(cmElements);
    }

    public E getCMElement(final String id) {
        Iterator<E> iterator = getCMElementByIds(Collections.singletonList(id)).iterator();
        if (!iterator.hasNext()) {
            throw new InstanceNotFoundException(id, getCMElementClass().getSimpleName());
        }
        return iterator.next();
    }

    public Collection<E> getCMElementsInCache(final Collection<String> ids) {
        Collection<E> cmElements = new ArrayList<E>();
        for (String id : ids) {
            cmElements.add(getCMElementInCache(id));
        }
        return cmElements;
    }

    public E getCMElementInCache(final String id) {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot load cache, it is not active."));
        }
        E cmElement = getCmElementMap().get(id);
        if (cmElement == null) {
            throw new InstanceNotFoundException(id, "CMElement");
        }
        return cmElement;
    }

    public Collection<E> getCMElementByIds(final Collection<String> ids) {
        if (ids == null) {
            throw new IllegalArgumentException("ids cannot have null value");
        }
        final Map<String, E> foundCMElementsMap = new HashMap<>();
        final Collection<String> idsNotInCache = new ArrayList<>();
        if (useCache) {
            findCMElementsInCache(ids, idsNotInCache, foundCMElementsMap);
        } else {
            idsNotInCache.addAll(ids);
        }
        findCMElementsNotInCache(idsNotInCache, foundCMElementsMap);
        List<E> foundCMElementsList = new ArrayList<>();
        for (String id : ids) {
            E cmElement = foundCMElementsMap.get(id);
            if (cmElement != null) {
                foundCMElementsList.add(cmElement);
            }
        }
        return foundCMElementsList;
    }

    private void findCMElementsInCache(Collection<String> ids, Collection<String> idsNotInCache, Map<String, E> foundCMElements) {
        checkForCacheUpdate();
        for (String id : ids) {
            E cmElement = getCmElementMap().get(id);
            if (cmElement != null) {
                foundCMElements.put(cmElement.getId(), cmElement);
            } else {
                idsNotInCache.add(id);
            }
        }
    }

    public void checkForCacheUpdate() {
        if (cachedCMManager.isDataChangedSince(lastCheckForChanges)) {
            lastCheckForChanges = Instant.now();
            logger.info("Detected changes on " + getCMElementClass().getSimpleName() + "). Initializing cache...");
            initialize();
        }
    }

    private void findCMElementsNotInCache(final Collection<String> idsNotInCache, final Map<String, E> foundCMElements) {
        if (!idsNotInCache.isEmpty()) {
            Collection<E> cmElementsNotFoundInCache = clinicalModelsService.searchCMElementsByIds(getCMElementClass(), idsNotInCache);
            for (E cmElementNotFoundInCache : cmElementsNotFoundInCache) {
                foundCMElements.put(cmElementNotFoundInCache.getId(), cmElementNotFoundInCache);
            }
            registerCMElementsInCache(cmElementsNotFoundInCache);
        }
    }

    public void registerCMElementsInCache(Collection<E> cmElements) {
        if (useCache) {
            for (E cmElement : cmElements) {
                getCmElementMap().put(cmElement.getId(), cmElement);
                cachedCMManager.renewLastUpdateDate(cmElement.getLastUpdate());
            }
        }
    }

    public void remove(String id) {
        clinicalModelsService.removeCMElement(getCMElementClass(), id);
    }

    public void upsert(E cmElement) {
        registerCMElementsInCache(Collections.singleton(cmElement));
    }

    public String getServerChecksum() {
        return clinicalModelsService.getChecksumForCMElements(getCMElementClass());
    }

    public String getCachedChecksum() {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot return checksum from cache if it is not used."));
        }
        return CheckSumManager.generateChecksum(getCmElementMap().values());
    }


    private synchronized Map<String, E> getCmElementMap() {
        if (cmElementMap == null) {
            cmElementMap = new HashMap<>();
        }
        return cmElementMap;
    }

    public boolean isDataChangedSince(Instant instant) {
        return cachedCMManager.isDataChangedSince(instant);
    }

    public void setUseCache(boolean useCache) {
        this.useCache = useCache;
    }

    public abstract Class<E> getCMElementClass();
}
