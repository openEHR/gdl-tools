package se.cambio.openehr.controller.session.data;

import org.apache.log4j.Logger;
import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.CMElementComparator;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class AbstractCMManager<E extends CMElement> {

    private static long MAX_CHECK_WAITING_TIME_IN_MILLIS = 5000; //5 seg
    private Map<String, E> cmElementMap;
    private boolean useCache = true;
    private Date lastCheckForUpdates;
    private Date mostRecentLocalUpdate;
    private Logger logger = Logger.getLogger(AbstractCMManager.class);

    public AbstractCMManager(){
        Date currentTime = Calendar.getInstance().getTime();
        mostRecentLocalUpdate = currentTime;
        lastCheckForUpdates = currentTime;
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
        if (!useCache){
            throw new InternalErrorException(new UnsupportedOperationException("Cannot cache, it is not active."));
        }
        return getCmElementMap().keySet();
    }

    public void loadAll() throws InternalErrorException {
        if (!useCache){
            throw new InternalErrorException(new UnsupportedOperationException("Cannot load into cache, it is not active."));
        }
        Collection<E> cmElements = OpenEHRSessionManager.getAdministrationFacadeDelegate().getAllCMElements(getCMElementClass());
        registerCMElementsInCache(cmElements);
    }

    public E getCMElement(final String id) throws InstanceNotFoundException, InternalErrorException {
        return getCMElementByIds(Collections.singleton(id)).iterator().next();
    }

    public Collection<E> getCMElementsInCache(final Collection<String> ids) throws InstanceNotFoundException, InternalErrorException {
        Collection<E> cmElements = new ArrayList<E>();
        for(String id: ids){
            cmElements.add(getCMElementInCache(id));
        }
        return cmElements;
    }

    public E getCMElementInCache(final String id) throws InstanceNotFoundException, InternalErrorException {
        if (!useCache){
            throw new InternalErrorException(new UnsupportedOperationException("Cannot load cache, it is not active."));
        }
        E cmElement = getCmElementMap().get(id);
        if (cmElement==null){
            throw new InstanceNotFoundException(id, "CMElement");
        }
        return cmElement;
    }

    public Collection<E> getCMElementByIds(final Collection<String> ids) throws InstanceNotFoundException, InternalErrorException {
        if (ids==null){
            throw new IllegalArgumentException("ids cannot have null value");
        }
        final Collection<E> foundCMElements = new ArrayList<E>();
        final Collection<String> idsNotInCache = new ArrayList<String>();
        if (useCache) {
            findCMElementsInCache(ids, idsNotInCache, foundCMElements);
        } else {
            idsNotInCache.addAll(ids);
        }
        findCMElementsNotInCache(idsNotInCache, foundCMElements);
        return foundCMElements;
    }

    private void findCMElementsInCache(Collection<String> ids, Collection<String> idsNotInCache, Collection<E> foundCMElements) throws InternalErrorException {
        cacheUpdate();
        for (String id: ids){
            E cmElement = getCmElementMap().get(id);
            if (cmElement!=null){
                foundCMElements.add(cmElement);
            }else{
                idsNotInCache.add(id);
            }
        }
    }

    public boolean cacheUpdate() throws InternalErrorException {
        boolean changesDetected = false;
        if (isWaitingTimeForNextCheckReached()) {
            changesDetected = isDataChanged(changesDetected);
        }
        if (changesDetected) {
            logger.info("Detected changes on " + getCMElementClass().getSimpleName() + "). Initializing cache...");
            initialize();
        }
        return changesDetected;
    }

    private boolean isDataChanged(boolean changesDetected) throws InternalErrorException {
        Date lastUpdateDateOnServer = OpenEHRSessionManager.getAdministrationFacadeDelegate().getLastUpdate(getCMElementClass());
        if (lastUpdateDateOnServer != null) {
            if (lastUpdateDateOnServer.getTime() > mostRecentLocalUpdate.getTime()) {
                changesDetected = true;
                mostRecentLocalUpdate = lastUpdateDateOnServer;
            }
        }
        return changesDetected;
    }

    private boolean isWaitingTimeForNextCheckReached() {
        boolean waitingTimeForNextCheckReached = false;
        long timeSinceLastCheck = System.currentTimeMillis() - lastCheckForUpdates.getTime();
        if (timeSinceLastCheck>=MAX_CHECK_WAITING_TIME_IN_MILLIS){
            waitingTimeForNextCheckReached = true;
            lastCheckForUpdates = Calendar.getInstance().getTime();
        }
        return waitingTimeForNextCheckReached;
    }

    private void findCMElementsNotInCache(final Collection<String> idsNotInCache, final Collection<E> foundCMElements) throws InternalErrorException, InstanceNotFoundException {
        if (!idsNotInCache.isEmpty()) {
            Collection<E> cmElementsNotFoundInCache = OpenEHRSessionManager.getAdministrationFacadeDelegate().searchCMElementsByIds(getCMElementClass(), idsNotInCache);
            foundCMElements.addAll(cmElementsNotFoundInCache);
            registerCMElementsInCache(cmElementsNotFoundInCache);
        }
    }

    public void registerCMElementsInCache(Collection<E> cmElements){
        if (useCache) {
            for (E cmElement : cmElements) {
                getCmElementMap().put(cmElement.getId(), cmElement);
                renewMostRecentLocalUpdate(cmElement.getLastUpdate());
            }
        }
    }

    private void renewMostRecentLocalUpdate(Date lastUpdate){
        if (lastUpdate!=null && lastUpdate.after(mostRecentLocalUpdate)){
            mostRecentLocalUpdate = lastUpdate;
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
        for(E cmElement : cmElementList){
            md.update(cmElement.getSource().getBytes());
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

    public <E extends CMElement>String getServerChecksum() throws InternalErrorException {
        return OpenEHRSessionManager.getAdministrationFacadeDelegate().getChecksumForCMElements(getCMElementClass());
    }

    public String getCachedChecksum() throws InternalErrorException {
        if (!useCache){
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

    public void setUseCache(boolean useCache){
        this.useCache = useCache;
    }

    public abstract Class<E> getCMElementClass();
}
