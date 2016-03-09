package se.cambio.openehr.controller.session.data;

import org.apache.log4j.Logger;
import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.CMElementComparator;
import se.cambio.openehr.util.CachedCMManager;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class AbstractCMManager<E extends CMElement> {

    private Map<String, E> cmElementMap;
    private boolean useCache = true;
    private Logger logger = Logger.getLogger(AbstractCMManager.class);
    private CachedCMManager cachedCMManager;

    public AbstractCMManager() {
        cachedCMManager = new CachedCMManager(getCMElementClass());
    }

    public void initialize() {
        getCmElementMap().clear();
    }

    public Collection<E> getAllInCache() {
        return getCmElementMap().values();
    }

    public Collection<String> getAllIds() throws InternalErrorException {
        return OpenEHRSessionManager.getAdministrationFacadeDelegate().getAllCMElementIds(getCMElementClass());
    }

    public Collection<String> getAllIdsInCache() throws InternalErrorException {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot cache, it is not active."));
        }
        return getCmElementMap().keySet();
    }

    public void loadAll() throws InternalErrorException {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot load into cache, it is not active."));
        }
        Collection<E> cmElements = OpenEHRSessionManager.getAdministrationFacadeDelegate().getAllCMElements(getCMElementClass());
        registerCMElementsInCache(cmElements);
    }

    public E getCMElement(final String id) throws InstanceNotFoundException, InternalErrorException {
        return getCMElementByIds(Collections.singletonList(id)).iterator().next();
    }

    public Collection<E> getCMElementsInCache(final Collection<String> ids) throws InstanceNotFoundException, InternalErrorException {
        Collection<E> cmElements = new ArrayList<E>();
        for (String id : ids) {
            cmElements.add(getCMElementInCache(id));
        }
        return cmElements;
    }

    public E getCMElementInCache(final String id) throws InstanceNotFoundException, InternalErrorException {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot load cache, it is not active."));
        }
        E cmElement = getCmElementMap().get(id);
        if (cmElement == null) {
            throw new InstanceNotFoundException(id, "CMElement");
        }
        return cmElement;
    }

    public Collection<E> getCMElementByIds(final Collection<String> ids) throws InstanceNotFoundException, InternalErrorException {
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

    private void findCMElementsInCache(Collection<String> ids, Collection<String> idsNotInCache, Map<String, E> foundCMElements) throws InternalErrorException {
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

    public void checkForCacheUpdate() throws InternalErrorException {
        if (cachedCMManager.isDataChanged()) {
            logger.info("Detected changes on " + getCMElementClass().getSimpleName() + "). Initializing cache...");
            initialize();
        }
    }

    private void findCMElementsNotInCache(final Collection<String> idsNotInCache, final Map<String, E> foundCMElements) throws InternalErrorException, InstanceNotFoundException {
        if (!idsNotInCache.isEmpty()) {
            Collection<E> cmElementsNotFoundInCache = OpenEHRSessionManager.getAdministrationFacadeDelegate().searchCMElementsByIds(getCMElementClass(), idsNotInCache);
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
                cachedCMManager.renewMostRecentLocalUpdate(cmElement.getLastUpdate());
            }
        }
    }

    public void remove(String id) throws InstanceNotFoundException, InternalErrorException {
        OpenEHRSessionManager.getAdministrationFacadeDelegate().removeCMElement(getCMElementClass(), id);
    }

    public void upsert(E cmElement) throws InternalErrorException {
        OpenEHRSessionManager.getAdministrationFacadeDelegate().upsertCMElement(cmElement);
        registerCMElementsInCache(Collections.singleton(cmElement));
    }

    public static <E extends CMElement> String generateChecksum(Collection<E> cmElements) throws InternalErrorException {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            List<E> cmElementList = getSortedCMElementList(cmElements);
            return getMD5Checksum(md, cmElementList);
        } catch (NoSuchAlgorithmException e) {
            throw new InternalErrorException(e);
        }
    }

    private static <E extends CMElement> String getMD5Checksum(MessageDigest md, List<E> cmElementList) {
        for (E cmElement : cmElementList) {
            try {
                md.update(cmElement.getSource().getBytes("UTF-8"));
            } catch (UnsupportedEncodingException e) {
                ExceptionHandler.handle(e);
            }
        }
        byte[] md5Bytes = md.digest();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < md5Bytes.length; i++) {
            sb.append(Integer.toString((md5Bytes[i] & 0xff) + 0x100, 16).substring(1));
        }
        return sb.toString();
    }

    private static <E extends CMElement> List<E> getSortedCMElementList(Collection<E> cmElements) {
        List<E> cmElementList = new ArrayList<E>();
        cmElementList.addAll(cmElements);
        Collections.sort(cmElementList, new CMElementComparator());
        return cmElementList;
    }

    public <E extends CMElement> String getServerChecksum() throws InternalErrorException {
        return OpenEHRSessionManager.getAdministrationFacadeDelegate().getChecksumForCMElements(getCMElementClass());
    }

    public String getCachedChecksum() throws InternalErrorException {
        if (!useCache) {
            throw new InternalErrorException(new UnsupportedOperationException("Cannot return checksum from cache if it is not used."));
        }
        return generateChecksum(getCmElementMap().values());
    }


    private synchronized Map<String, E> getCmElementMap() {
        if (cmElementMap == null) {
            cmElementMap = new HashMap<String, E>();
        }
        return cmElementMap;
    }

    public void setUseCache(boolean useCache) {
        this.useCache = useCache;
    }

    public abstract Class<E> getCMElementClass();
}
