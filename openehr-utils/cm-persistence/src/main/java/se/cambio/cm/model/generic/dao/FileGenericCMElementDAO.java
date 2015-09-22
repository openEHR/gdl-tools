package se.cambio.cm.model.generic.dao;

import se.cambio.cm.model.util.CMElement;
import se.cambio.cm.model.util.CMTypeManager;
import se.cambio.openehr.util.CmFolder;
import se.cambio.openehr.util.ExceptionHandler;
import org.apache.commons.io.IOUtils;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.exceptions.FolderNotFoundException;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

public class FileGenericCMElementDAO <E extends CMElement> implements GenericCMElementDAO<E>{

    private Class<E> cmElementClass;
    private CmFolder cmFolder;
    private Collection<String> fileExtensions;

    public FileGenericCMElementDAO(Class<E> cmElementClass, CmFolder cmFolder) {
        this.cmElementClass = cmElementClass;
        this.cmFolder = cmFolder;
    }

    @Override
    public Collection<E> searchByIds(Collection<String> ids) throws InternalErrorException, InstanceNotFoundException {
        Collection<E> cmElements = searchAll();
        Collection<E> foundCMElements = new ArrayList<E>();
        for (E cmElement : cmElements) {
            if (ids.contains(cmElement.getId())) {
                foundCMElements.add(cmElement);
            }
        }
        checkMissingInstance(ids, foundCMElements);
        return foundCMElements;
    }

    @Override
    public Collection<String> searchAllIds() throws InternalErrorException {
        if (!getFolder().isDirectory()) {
            throw new FolderNotFoundException(getFolder().getAbsolutePath());
        }
        Collection<String> ids = new ArrayList<String>();
        File[] listOfFiles = getFolder().listFiles();
		if(listOfFiles != null) {
			for (int i = 0; i < listOfFiles.length; i++) {
				if (listOfFiles[i].isFile()) {
					String fileName = listOfFiles[i].getName();
					String fileExtension = matchingFileExtension(fileName);
					if (fileExtension!=null) {
						String id = getId(fileName, fileExtension);
						ids.add(id);
					}
				}
			}
		}
        return ids;
    }

    @Override
    public Collection<E> searchAll() throws InternalErrorException {
        Collection<E> cmElements = new ArrayList<E>();
        if (!getFolder().isDirectory()) {
            throw new FolderNotFoundException(getFolder().getAbsolutePath());
        }
        File[] listOfFiles = getFolder().listFiles();
		if(listOfFiles != null) {
			for (int i = 0; i < listOfFiles.length; i++) {
				File file = listOfFiles[i];
				if (file.isFile()) {
					String fileName = file.getName();
					String fileExtension = matchingFileExtension(fileName);
					if (fileExtension!=null) {
						try {
							FileInputStream fis = new FileInputStream(file.getAbsolutePath());
							try {
								E cmElement = getCMElement(fileName, fileExtension, fis, new Date(file.lastModified()));
								cmElements.add(cmElement);
							}finally{
								fis.close();
							}
						} catch (Exception e) {
							ExceptionHandler.handle(e);
						}
					}
				}
			}
		}
        return cmElements;
    }

    public E getCMElement(String fileName, String fileExtension, InputStream fis, Date date) throws IOException, InternalErrorException {
        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
        ubis.skipBOM();
        String id = getId(fileName, fileExtension);
        String src = IOUtils.toString(ubis, "UTF-8");
        return (E)new CMElementBuilder<E>()
                        .setId(id)
                        .setFormat(fileExtension)
                        .setSource(src)
                        .setLastUpdate(date)
                        .createCMElement(cmElementClass);
    }

    private String getId(String fileName, String fileExtension) {
        return fileName.substring(0, fileName.length() - fileExtension.length() - 1);
    }

    private String matchingFileExtension(String fileName) throws InternalErrorException {
        for(String fileExtension: getFileExtensions()){
            if (fileName.endsWith("."+fileExtension)){
                return fileExtension;
            }
        }
        return null;
    }

    @Override
    public void insert(E cmElement) throws InternalErrorException {
        upsert(cmElement);
    }

    @Override
    public void update(E cmElement) throws InternalErrorException {
        upsert(cmElement);
    }

    private void upsert(E cmElement) throws InternalErrorException {
        if (!getFolder().isDirectory()) {
            throw new FolderNotFoundException(getFolder().getAbsolutePath());
        }
        File file = new File(getFolder(), cmElement.getId() + "." + cmElement.getFormat());
        try {
            Files.write(Paths.get(file.toURI()), cmElement.getSource().getBytes("UTF-8"));
        } catch (IOException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void remove(String id) throws InternalErrorException {
        if (!getFolder().isDirectory()) {
            throw new FolderNotFoundException(getFolder().getAbsolutePath());
        }
        File file = new File(getFolder(), id + "." + getFileExtensions().iterator().next());
        file.delete();
    }

    @Override
    public void removeAll() throws InternalErrorException {
        File[] listOfFiles = getFolder().listFiles();
		if(listOfFiles != null) {
			for (int i = 0; i < listOfFiles.length; i++) {
				File file = listOfFiles[i];
				String matchingExtension = matchingFileExtension(file.getName());
				if (matchingExtension != null){
					String id = getId(file.getName(), matchingExtension);
					remove(id);
				}
			}
		}
    }

    @Override
    public Date getLastUpdateDate() throws InternalErrorException {
        if (!getFolder().isDirectory()) {
            throw new FolderNotFoundException(getFolder().getAbsolutePath());
        }
        Date lastModifiedDate = null;
        File[] listOfFiles = getFolder().listFiles();
		if(listOfFiles != null) {
			for (int i = 0; i < listOfFiles.length; i++) {
				File file = listOfFiles[i];
				Date date = new Date(file.lastModified());
				if (lastModifiedDate == null || date.after(lastModifiedDate)){
					lastModifiedDate = date;
				}
			}
		}
        return lastModifiedDate;
    }

    private File getFolder() {
        return cmFolder.getFolder();
    }

    private void checkMissingInstance(Collection<String> ids, Collection<E> cmElements) throws InstanceNotFoundException {
        Collection<String> foundIds = new ArrayList<String>();
        for (CMElement cmElement: cmElements){
            foundIds.add(cmElement.getId());
        }
        for(String id: ids){
            if (!foundIds.contains(id)){
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