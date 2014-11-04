package se.cambio.cds.util;

import org.apache.log4j.Logger;
import se.cambio.cm.model.cm.element.dao.CMElementBuilder;
import se.cambio.cm.model.facade.administration.delegate.CMAdministrationFacadeDelegate;
import se.cambio.cm.model.util.CMElement;
import se.cambio.cm.model.util.CMType;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.*;
import java.nio.charset.Charset;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * User: Iago.Corbal
 * Date: 2013-10-28
 * Time: 17:28
 */
public class CMImportExportManager {

    public CMImportExportManager() {
    }

    public void exportCurrentCM(OutputStream outputStream) throws IOException, InstanceNotFoundException, InternalErrorException {
        ZipOutputStream out = new ZipOutputStream(outputStream, Charset.forName("UTF-8"));
        for(CMType cmType: CMType.values()){
            export(cmType, out);
        }
        out.close();
    }

    private void export(CMType cmType, ZipOutputStream out) throws IOException, InternalErrorException, InstanceNotFoundException {
        CMAdministrationFacadeDelegate cmAFD = OpenEHRSessionManager.getAdministrationFacadeDelegate();
        Collection<? extends CMElement> cmElements = cmAFD.getAllCMElements(cmType.getCmElementClass());
        for (CMElement cmElement: cmElements){
            out.putNextEntry(new ZipEntry(cmType.getId() + "\\" + cmElement.getId() + "." + cmType.getFileExtensions().iterator().next()));
            IOUtils.write(cmElement.getSource(), out, "UTF-8");
        }
    }

    public void importCM(InputStream is) throws IOException, InternalErrorException {
        CMAdministrationFacadeDelegate cmAFD = OpenEHRSessionManager.getAdministrationFacadeDelegate();
        BufferedInputStream bis = new BufferedInputStream(is);
        ZipInputStream zis = new ZipInputStream(bis, Charset.forName("UTF-8"));
        Map<String, CMType> fileExtensionCMTypeMap = getFileExtensionCMTypeMap();
        Map<CMType, Collection<CMElement>> cmElementMap = new HashMap<CMType, Collection<CMElement>>();
        try{
            ZipEntry entry;
            while((entry = zis.getNextEntry())!=null) {
                int lastDotIndex = entry.getName().lastIndexOf(".");
                if (lastDotIndex<0){
                    Logger.getLogger(CMImportExportManager.class).warn("Skipping '"+entry.getName()+"' (invalid extension)");
                    continue;
                }
                String extension = entry.getName().substring(lastDotIndex + 1, entry.getName().length());
                CMType cmType = fileExtensionCMTypeMap.get(extension);
                if (cmType == null){
                    Logger.getLogger(CMImportExportManager.class).warn("Skipping '"+entry.getName()+"' (unkown extension)");
                    continue;
                }
                int lastSlashIndex = entry.getName().lastIndexOf("\\");
                if (lastSlashIndex<0){
                    lastSlashIndex = entry.getName().lastIndexOf("/");
                }
                if (lastSlashIndex<0){
                    Logger.getLogger(CMImportExportManager.class).warn("Skipping '"+entry.getName()+"' (invalid directory structure)");
                    continue;
                }
                String id = entry.getName().substring(lastSlashIndex+1, lastDotIndex);
                String src = IOUtils.toString(zis, "UTF-8");
                CMElement cmElement = new CMElementBuilder().build(cmType.getCmElementClass());
                cmElement.setId(id);
                cmElement.setSource(src);
                cmElement.setLastUpdate(new Date(entry.getTime()));
                insertCMElementIntoMap(cmElementMap, cmType, cmElement);
            }
            CMElementProcessor cmElementProcessor = new CMElementProcessor();
            for (CMType cmType: CMType.values()){
                Collection<CMElement> cmElements = cmElementMap.get(cmType);
                if (cmElements != null) {
                    for (CMElement cmElement : cmElements) {
                        cmElementProcessor.process(cmElement);
                        cmAFD.upsertCMElement(cmElement);
                    }
                }
            }
        }finally{
            // we must always close the zip file.
            zis.close();
        }
    }

    private void insertCMElementIntoMap(Map<CMType, Collection<CMElement>> cmElementMap, CMType cmType, CMElement cmElement) {
        Collection<CMElement> cmElements = cmElementMap.get(cmType);
        if (cmElements == null){
            cmElements = new ArrayList<CMElement>();
            cmElementMap.put(cmType, cmElements);
        }
        cmElements.add(cmElement);
    }

    private Map<String, CMType> getFileExtensionCMTypeMap(){
        Map<String, CMType> fileExtensionCMTypeMap = new HashMap<String, CMType>();
        for(CMType cmType: CMType.values()){
            for(String fileExtension: cmType.getFileExtensions()){
                fileExtensionCMTypeMap.put(fileExtension, cmType);
            }
        }
        return fileExtensionCMTypeMap;
    }
}