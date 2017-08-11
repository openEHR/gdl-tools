package se.cambio.cm.model.generic.dao;

import org.apache.commons.io.IOUtils;
import org.jberet.support.io.UnicodeBOMInputStream;
import se.cambio.cm.model.util.CMElement;
import se.cambio.cm.model.util.CMTypeManager;
import se.cambio.openehr.util.Resources;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

public class ResourceGenericCMElementDAO<E extends CMElement> implements GenericCMElementDAO<E> {

    private Class<E> cmElementClass;
    private Collection<String> fileExtensions;

    public ResourceGenericCMElementDAO(Class<E> cmElementClass) {
        this.cmElementClass = cmElementClass;
    }

    @Override
    public Collection<E> searchByIds(Collection<String> ids) throws InternalErrorException, InstanceNotFoundException {
        Collection<E> cmElements = new ArrayList<E>();
        InputStream is = getInputStreamForResourceList();
        if (is != null) {
            Collection<String> resourceFileNamesIdsMap = getResourceFileNames(is);
            for (String resourceFileName : resourceFileNamesIdsMap) {
                try {
                    String fileExtension = matchingFileExtension(resourceFileName);
                    String id = resourceFileName.substring(resourceFileName.lastIndexOf("/") + 1, resourceFileName.length() - fileExtension.length() - 1);
                    if (ids.contains(id)) {
                        E cmElement = getCMElement(resourceFileName);
                        cmElements.add(cmElement);
                    }
                } catch (Exception ex) {
                    throw new InternalErrorException(ex);
                }
            }
        } else {
            throw new InternalErrorException(new Exception("Resource list not found!"));
        }
        checkMissingInstance(ids, cmElements);
        return cmElements;
    }

    @Override
    public Collection<E> searchAll() throws InternalErrorException {
        Collection<E> cmElements = new ArrayList<E>();
        InputStream is = getInputStreamForResourceList();
        if (is != null) {
            Collection<String> resourceFileNamesIdsMap = getResourceFileNames(is);
            for (String resourceFileName : resourceFileNamesIdsMap) {
                try {
                    E cmElement = getCMElement(resourceFileName);
                    cmElements.add(cmElement);
                } catch (Exception ex) {
                    throw new InternalErrorException(ex);
                }
            }
        } else {
            throw new InternalErrorException(new Exception("Resource list not found!"));
        }
        return cmElements;
    }

    private E getCMElement(String resourceFileName) throws IOException, InternalErrorException {
        InputStream fis = ResourceGenericCMElementDAO.class.getClassLoader().getResourceAsStream(resourceFileName);
        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
        ubis.skipBOM();
        String fileExtension = matchingFileExtension(resourceFileName);
        String id = resourceFileName.substring(resourceFileName.lastIndexOf("/") + 1, resourceFileName.length() - fileExtension.length() - 1);
        String source = IOUtils.toString(ubis, "UTF-8");
        E cmElement = (E) new CMElementBuilder<E>()
                .setId(id)
                .setFormat(fileExtension)
                .setSource(source)
                .setLastUpdate(Calendar.getInstance().getTime())
                .createCMElement(cmElementClass);
        return cmElement;
    }

    private InputStream getInputStreamForResourceList() throws InternalErrorException {
        return ResourceGenericCMElementDAO.class.getClassLoader().getResourceAsStream(Resources.RESOURCES_LIST);
    }

    private Collection<String> getResourceFileNames(InputStream is) throws InternalErrorException {
        Collection<String> resourceFileNames = new ArrayList<String>();
        try {
            String resourceList = IOUtils.toString(is, "UTF-8");
            for (String resourceFileName : resourceList.split("\n")) {
                resourceFileName = resourceFileName.trim();
                String fileExtension = matchingFileExtension(resourceFileName);
                if (fileExtension != null) {
                    //Remove the leading '\'
                    resourceFileName = resourceFileName.replaceAll("\\\\", "/");
                    resourceFileNames.add(resourceFileName.substring(1, resourceFileName.length()));
                }
            }
        } catch (IOException ex) {
            throw new InternalErrorException(ex);
        }
        return resourceFileNames;
    }

    @Override
    public Collection<String> searchAllIds() throws InternalErrorException {
        Collection<String> ids = new ArrayList<>();
        InputStream is = getInputStreamForResourceList();
        Collection<String> resourceFileNames = getResourceFileNames(is);
        for (String resourceFileName : resourceFileNames) {
            String fileExtension = matchingFileExtension(resourceFileName);
            String id = resourceFileName.substring(resourceFileName.lastIndexOf("/") + 1, resourceFileName.length() - fileExtension.length() - 1);
            ids.add(id);
        }
        return ids;
    }

    @Override
    public void insert(E cmElement) throws InternalErrorException {
        throw new UnsupportedOperationException("Not allowed on resources DAO");
    }

    @Override
    public void update(E cmElement) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException("Not allowed on resources DAO");
    }

    @Override
    public void remove(String id) throws InternalErrorException, InstanceNotFoundException {
        throw new UnsupportedOperationException("Not allowed on resources DAO");
    }

    @Override
    public void removeAll() throws InternalErrorException {
        throw new UnsupportedOperationException("Not allowed on resources DAO");
    }

    @Override
    public Date getLastUpdateDate() throws InternalErrorException {
        return Calendar.getInstance().getTime();
    }

    private String matchingFileExtension(String fileName) throws InternalErrorException {
        for (String fileExtension : getFileExtensions()) {
            if (fileName.endsWith("." + fileExtension)) {
                return fileExtension;
            }
        }
        return null;
    }

    private void checkMissingInstance(Collection<String> ids, Collection<E> cmElements) throws InstanceNotFoundException {
        Collection<String> foundIds = new ArrayList<>();
        for (CMElement cmElement : cmElements) {
            foundIds.add(cmElement.getId());
        }
        for (String id : ids) {
            if (!foundIds.contains(id)) {
                throw new InstanceNotFoundException(id, getCMElementClassName());
            }
        }
    }

    private String getCMElementClassName() {
        return getCMElementClass().getSimpleName();
    }

    private Class<E> getCMElementClass() {
        return cmElementClass;
    }

    public Collection<String> getFileExtensions() throws InternalErrorException {
        if (fileExtensions == null) {
            fileExtensions = CMTypeManager.getInstance().getCMTypeByClass(getCMElementClass()).getFileExtensions();
        }
        return fileExtensions;
    }
}
