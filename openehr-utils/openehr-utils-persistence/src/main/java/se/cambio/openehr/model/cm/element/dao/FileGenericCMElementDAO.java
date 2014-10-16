package se.cambio.openehr.model.cm.element.dao;

import se.cambio.openehr.model.util.CMElement;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
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

public class FileGenericCMElementDAO<E extends CMElement> implements GenericCMElementDAO<E>{

    private Class<E> cmElementClass;
    private File folder;
    private Collection<String> fileExtensions;

    public FileGenericCMElementDAO(Class<E> cmElementClass, File folder, Collection<String> fileExtensions){
        this.cmElementClass = cmElementClass;
        this.folder = folder;
        this.fileExtensions = fileExtensions;
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
        //TODO Instance not found exception if ids are not found
        return foundCMElements;
    }

    @Override
    public Collection<String> searchAllIds() throws InternalErrorException {
        if (!folder.isDirectory()) {
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        Collection<String> ids = new ArrayList<String>();
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            if (listOfFiles[i].isFile()) {
                String fileName = listOfFiles[i].getName();
                String fileExtension = matchingFileExtension(fileName);
                if (fileExtension!=null) {
                    String id = fileName.substring(0, fileName.length() - fileExtension.length() - 1);
                    ids.add(id);
                }
            }
        }
        return ids;
    }

    @Override
    public Collection<E> searchAll() throws InternalErrorException {
        Collection<E> cmElements = new ArrayList<E>();
        if (!folder.isDirectory()) {
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            File file = listOfFiles[i];
            if (file.isFile()) {
                String fileName = file.getName();
                String fileExtension = matchingFileExtension(fileName);
                if (fileExtension!=null) {
                    try {
                        InputStream fis = new FileInputStream(file.getAbsolutePath());
                        UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                        ubis.skipBOM();
                        String id = fileName.substring(0, fileName.length() - fileExtension.length() - 1);
                        String src = IOUtils.toString(ubis, "UTF-8");
                        E cmElement = new CMElementBuilder<E>().build(cmElementClass);
                        cmElement.setId(id);
                        cmElement.setSource(src);
                        cmElement.setLastUpdate(new Date(file.lastModified()));
                        cmElements.add(cmElement);
                    } catch (Exception e) {
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
        return cmElements;
    }

    private String matchingFileExtension(String fileName){
        for(String fileExtension: fileExtensions){
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
        File file = new File(folder, cmElement.getId()+"."+fileExtensions.iterator().next());
        try {
            Files.write(Paths.get(file.toURI()), cmElement.getSource().getBytes());
        } catch (IOException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public void remove(String id) throws InternalErrorException, InstanceNotFoundException {
        File file = new File(folder, id+"."+fileExtensions.iterator().next());
        file.delete();
    }

    @Override
    public Date getLastUpdateDate() throws InternalErrorException {
        Collection<E> cmElements = new ArrayList<E>();
        if (!folder.isDirectory()) {
            throw new FolderNotFoundException(folder.getAbsolutePath());
        }
        Date lastModifiedDate = null;
        File[] listOfFiles = folder.listFiles();
        for (int i = 0; i < listOfFiles.length; i++) {
            File file = listOfFiles[i];
            Date date = new Date(file.lastModified());
            if (lastModifiedDate == null || date.after(lastModifiedDate)){
                lastModifiedDate = date;
            }
        }
        return lastModifiedDate;
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