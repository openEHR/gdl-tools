package se.cambio.openehr.view.util;

import openEHR.v1.template.TEMPLATE;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.template.Flattener;
import org.openehr.am.template.FlatteningException;
import org.openehr.am.template.UnknownArchetypeException;
import se.cambio.openehr.controller.TemplateObjectBundleManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.template.dto.TemplateDTOBuilder;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice.MessageType;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;

public class ImportUtils {

    public static int showImportArchetypeDialogAndAddToRepo(Window owner, File selectedFile){
        JFileChooser fileChooser = getArchetypeFileChooser(selectedFile);
        int result = fileChooser.showOpenDialog(owner);
        if (result != JFileChooser.CANCEL_OPTION){
            addArchetype(owner, fileChooser.getSelectedFile());
        }
        return result;
    }

    public static JFileChooser getArchetypeFileChooser(File selectedFile){
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter(
                OpenEHRLanguageManager.getMessage("Archetype"),new String[]{"adl"});
        fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ImportArchetype"));
        fileChooser.setFileFilter(filter);
        if (selectedFile!=null){
            fileChooser.setSelectedFile(selectedFile);
        }
        return fileChooser;
    }

    public static int showImportTemplateDialog(Window owner, File selectedFile){
        JFileChooser fileChooser = getTemplateFileChooser(selectedFile);
        int result = fileChooser.showOpenDialog(owner);
        if (result != JFileChooser.CANCEL_OPTION){
            addTemplate(owner, fileChooser.getSelectedFile());
        }
        return result;
    }

    public static JFileChooser getTemplateFileChooser(File selectedFile){
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter(
                OpenEHRLanguageManager.getMessage("Template"),new String[]{"oet"});
        fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ImportTemplate"));
        fileChooser.setFileFilter(filter);
        if (selectedFile!=null){
            fileChooser.setSelectedFile(selectedFile);
        }
        return fileChooser;
    }

    //TODO Should be on a SW
    private static void addArchetype(final Window owner, File file){
        String fileName = file.getName().toLowerCase();
        if (fileName.endsWith(".adl")){
            try{
                ArchetypeDTO archetypeDTO = getArchetypeDTOFromFile(file);
                Archetypes.loadArchetype(archetypeDTO);
            }catch(final Exception e){
                ExceptionHandler.handle(e);
                try {
                    SwingUtilities.invokeAndWait(new Runnable() {
                        @Override
                        public void run() {
                            DialogLongMessageNotice dialog =
                                    new DialogLongMessageNotice(
                                            owner,
                                            OpenEHRLanguageManager.getMessage("ErrorParsingArchetypeT"),
                                            OpenEHRLanguageManager.getMessage("ErrorParsingArchetype"),
                                            e.getMessage(),
                                            MessageType.ERROR
                                    );
                            dialog.setVisible(true);
                        }
                    });
                } catch (InterruptedException e1) {
                    ExceptionHandler.handle(e);
                } catch (InvocationTargetException e1) {
                    ExceptionHandler.handle(e);
                }
            }
        }
    }

    public static ArchetypeDTO getArchetypeDTOFromFile(File file) throws InternalErrorException{
        InputStream fis = null;
        try{
            fis = new FileInputStream(file);
            UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
            ubis.skipBOM();
            String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
            String fileName = file.getName();
            String idArchetype = fileName.substring(0, fileName.length()-4);
            ArchetypeDTO archetypeDTO =
                    new ArchetypeDTO(idArchetype, idArchetype, idArchetype, null, archetypeSrc, null, null);
            return archetypeDTO;
        } catch (Exception e) {
            throw new InternalErrorException(e);
        } catch (Error e) {
            throw new InternalErrorException(new Exception(e.getMessage()));
        } finally{
            if (fis!=null){
                try {
                    fis.close();
                } catch (IOException e) {
                    ExceptionHandler.handle(e);
                }
            }
        }
    }

    private static void addTemplate(final Window owner, File file){
        String fileName = file.getName().toLowerCase();
        InputStream fis = null;
        if (fileName.endsWith(".oet")){
            try{
                fis = new FileInputStream(file.getAbsolutePath());
                UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
                ubis.skipBOM();
                String idTemplate = fileName.substring(0,fileName.length()-4);
                String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
                importTemplate(owner, idTemplate, archetypeSrc);
            }catch(final Exception e){
                ExceptionHandler.handle(e);
                try {
                    SwingUtilities.invokeAndWait(new Runnable() {
                        @Override
                        public void run() {
                            DialogLongMessageNotice dialog =
                                    new DialogLongMessageNotice(
                                            owner,
                                            OpenEHRLanguageManager.getMessage("ErrorParsingTemplateT"),
                                            OpenEHRLanguageManager.getMessage("ErrorParsingTemplate"),
                                            e.getMessage(),
                                            MessageType.ERROR
                                    );
                            dialog.setVisible(true);
                        }
                    });
                } catch (InterruptedException e1) {
                    ExceptionHandler.handle(e);
                } catch (InvocationTargetException e1) {
                    ExceptionHandler.handle(e1);
                }
            }finally{
                if (fis!=null){
                    try {
                        fis.close();
                    } catch (IOException e) {
                        ExceptionHandler.handle(e);
                    }
                }
            }
        }
    }

    public static TemplateDTO importTemplate(Window owner, String idTemplate, String archetypeSrc) throws Exception{
        TemplateDTO templateDTO = new TemplateDTOBuilder()
                .setTemplateId(idTemplate)
                .setArcehtypeId(idTemplate)
                .setName(idTemplate)
                .setDescription(idTemplate)
                .setArchetype(archetypeSrc)
                .createTemplateDTO();
        TEMPLATE template = TemplateObjectBundleManager.getParsedTemplate(templateDTO.getArchetype());
        Map<String, Archetype> archetypeMap = null;
        boolean lookupForArchetypes = true;
        while(lookupForArchetypes){
            lookupForArchetypes = false;
            archetypeMap = Archetypes.getArchetypeMap();
            String missingArchetypeId = null;
            try{
                new Flattener().toFlattenedArchetype(template, archetypeMap);
            }catch(FlatteningException e){
                if (e instanceof UnknownArchetypeException){
                    missingArchetypeId = ((UnknownArchetypeException)e).getArchetypeId();
                }else{
                    throw e;
                }
            }
            if (missingArchetypeId!=null){
                lookupForArchetypes=true;
                int result = showImportArchetypeDialogAndAddToRepo(owner, new File(missingArchetypeId + ".adl"));
                if (result==JFileChooser.CANCEL_OPTION){
                    return null;
                }
            }
        }
        String idArchetype = template.getDefinition().getArchetypeId();
        templateDTO.setArcehtypeId(idArchetype);
        templateDTO.setName(template.getName());
        //TODO
        if (template.getDescription()!=null && template.getDescription().getDetails()!=null&&template.getDescription().getDetails().getPurpose()!=null){
            templateDTO.setDescription(template.getDescription().getDetails().getPurpose());
        }
        Templates.loadTemplate(templateDTO);
        return templateDTO;
    }

    public static TemplateDTO getTemplateDTOFromFile(File file) throws InternalErrorException{
        InputStream fis = null;
        try{
            fis = new FileInputStream(file);
            UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
            ubis.skipBOM();
            String archetypeSrc = IOUtils.toString(ubis, "UTF-8");
            String fileName = file.getName();
            String idTemplate = fileName.substring(0,fileName.length()-4);
            TemplateDTO templateDTO = new TemplateDTOBuilder()
                    .setTemplateId(idTemplate)
                    .setArcehtypeId(idTemplate)
                    .setName(idTemplate)
                    .setDescription(idTemplate)
                    .setArchetype(archetypeSrc)
                    .createTemplateDTO();
            new TemplateObjectBundleManager(templateDTO).generateArchetypeObjectBundleCustomVO();
            return templateDTO;
        } catch (Exception e) {
            throw new InternalErrorException(e);
        } finally{
            if (fis!=null){
                try {
                    fis.close();
                } catch (IOException e) {
                    ExceptionHandler.handle(e);
                }
            }
        }
    }


    public static TerminologyDTO getTerminologyDTOFromFile(File file) throws InternalErrorException{
        InputStream fis = null;
        try{
            fis = new FileInputStream(file);
            UnicodeBOMInputStream ubis = new UnicodeBOMInputStream(fis);
            ubis.skipBOM();
            byte[] termSetSrc = IOUtils.toByteArray(ubis);
            String fileName = file.getName();
            String terminologyId = fileName.substring(0,fileName.length()-4);
            TerminologyDTO terminologyDTO = new TerminologyDTO(terminologyId,termSetSrc);
            return terminologyDTO;
        } catch (Exception e) {
            throw new InternalErrorException(e);
        } finally{
            if (fis!=null){
                try {
                    fis.close();
                } catch (IOException e) {
                    ExceptionHandler.handle(e);
                }
            }
        }
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