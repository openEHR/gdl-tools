package se.cambio.cds.controller.session.data;

import se.cambio.cds.controller.CDSSessionManager;
import se.cambio.cds.util.comparators.CMElementComparator;
import se.cambio.openehr.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.lang.reflect.ParameterizedType;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

public abstract class AbstractCMManager<E extends CMElement> {

    private Map<String, E> cmElementMap;
    private boolean useCache = true;

    public AbstractCMManager(){

    }

    private Class<E> getCMElementClass() {
        return (Class<E>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
    }

    public void initialize() {
        getCmElementMap().clear();
    }

    public Collection<String> getAllIds() throws InternalErrorException {
        return CDSSessionManager.getAdministrationFacadeDelegate().getAllCMElementIds(getCMElementClass());
    }

    public void loadAll() throws InternalErrorException {
        if (!useCache){
            throw new InternalErrorException(new UnsupportedOperationException("Cannot load into cache, it is not active."));
        }
        Collection<E> cmElements = CDSSessionManager.getAdministrationFacadeDelegate().getAllCMElements(getCMElementClass());
        registerCMElementsIfCacheUsed(cmElements);
    }

    public Collection<E> getCMElementByIds(final Collection<String> ids) throws InstanceNotFoundException, InternalErrorException {
        Collection<E> foundCMElements = new ArrayList<E>();
        Collection<String> idsNotInCache = new ArrayList<String>();
        finCMElementsInCache(ids, idsNotInCache, foundCMElements);
        findCMElementsNotInCache(idsNotInCache, foundCMElements);
        return foundCMElements;
    }

    private void finCMElementsInCache(Collection<String> ids, Collection<String> idsNotInCache, Collection<E> foundCMElements) {
        for (String id: ids){
            E cmElement = getCmElementMap().get(id);
            if (cmElement!=null){
                foundCMElements.add(cmElement);
            }else{
                idsNotInCache.add(id);
            }
        }
    }

    private void findCMElementsNotInCache(final Collection<String> idsNotInCache, final Collection<E> foundCMElements) throws InternalErrorException, InstanceNotFoundException {
        if (!idsNotInCache.isEmpty()) {
            Collection<E> cmElementsNotFoundInCache = CDSSessionManager.getAdministrationFacadeDelegate().searchCMElementsByIds(getCMElementClass(), idsNotInCache);
            foundCMElements.addAll(cmElementsNotFoundInCache);
            registerCMElementsIfCacheUsed(cmElementsNotFoundInCache);
        }
    }

    private void registerCMElementsIfCacheUsed(Collection<E> cmElements){
        if (useCache) {
            for (E cmElement : cmElements) {
                getCmElementMap().put(cmElement.getId(), cmElement);
            }
        }
    }

    public void remove(String id) throws InstanceNotFoundException, InternalErrorException {
        CDSSessionManager.getAdministrationFacadeDelegate().removeCMElement(getCMElementClass(), id);
    }

    public void upsert(E cmElement) throws InternalErrorException {
        CDSSessionManager.getAdministrationFacadeDelegate().upsertCMElement(cmElement);
    }

    public static <E extends CMElement>String generateChecksum(Collection<E> cmElements) throws InternalErrorException {
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
        return CDSSessionManager.getAdministrationFacadeDelegate().getChecksumForCMElements(getCMElementClass());
    }

    public String getCachedChecksum() throws InternalErrorException {
        if (!useCache){
            throw new InternalErrorException(new UnsupportedOperationException("Cannot return checksum from cache if it is not used."));
        }
        return generateChecksum(getCmElementMap().values());
    }

    private Map<String, E> getCmElementMap() {
        if (cmElementMap == null) {
            cmElementMap = new HashMap<String, E>();
        }
        return cmElementMap;
    }

    public void setUseCache(boolean useCache){
        this.useCache = useCache;
    }
}
