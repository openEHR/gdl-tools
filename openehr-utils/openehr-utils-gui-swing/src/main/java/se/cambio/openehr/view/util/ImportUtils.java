package se.cambio.openehr.view.util;

import openEHR.v1.template.TEMPLATE;
import org.openehr.am.archetype.Archetype;
import org.openehr.am.template.OETParser;
import se.acode.openehr.parser.ParseException;
import se.cambio.openehr.controller.OpenEHRObjectBundleManager;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.UnicodeBOMInputStream;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.MissingConfigurationParameterException;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice.MessageType;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.*;
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
    private static void addArchetype(Window owner, File file){
        String fileName = file.getName().toLowerCase();
        if (fileName.endsWith(".adl")){
            try{
                ArchetypeDTO archetypeDTO = getArchetypeDTOFromFile(file);
                OpenEHRObjectBundleManager.addArchetype(archetypeDTO);
                Archetypes.loadArchetypeDTO(archetypeDTO);
            }catch(Exception e){
                ExceptionHandler.handle(e);
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
            OpenEHRObjectBundleManager.generateArchetypeObjectBundleCustomVO(archetypeDTO);
            return archetypeDTO;
        }catch(MissingConfigurationParameterException e){
            throw new InternalErrorException(e);
        } catch (FileNotFoundException e) {
            throw new InternalErrorException(e);
        } catch (ParseException e) {
            throw new InternalErrorException(e);
        } catch (IOException e) {
            throw new InternalErrorException(e);
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

    private static void addTemplate(Window owner, File file){
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
            }catch(Exception e){
                ExceptionHandler.handle(e);
                DialogLongMessageNotice dialog =
                        new DialogLongMessageNotice(
                                owner,
                                OpenEHRLanguageManager.getMessage("ErrorParsingTemplateT"),
                                OpenEHRLanguageManager.getMessage("ErrorParsingTemplate"),
                                e.getMessage(),
                                MessageType.ERROR
                        );
                dialog.setVisible(true);
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
        TemplateDTO templateDTO =
                new TemplateDTO(idTemplate, idTemplate, idTemplate, idTemplate, null, archetypeSrc, null, null);
        OETParser parser = new OETParser();
        InputStream bis = new ByteArrayInputStream(templateDTO.getArchetype().getBytes());
        TEMPLATE template = parser.parseTemplate(bis).getTemplate();
        String idArchetype = template.getDefinition().getArchetypeId();
        ArchetypeDTO archetypeVO = Archetypes.getArchetypeDTO(idArchetype);
        if (archetypeVO==null){
            int result = showImportArchetypeDialogAndAddToRepo(owner, new File(idArchetype + ".adl"));
            if (result==JFileChooser.CANCEL_OPTION){
                return null;
            }
        }
        templateDTO.setIdArchetype(idArchetype);
        templateDTO.setName(template.getName());
        //TODO
        if (template.getDescription()!=null && template.getDescription().getDetails()!=null&&template.getDescription().getDetails().getPurpose()!=null){
            templateDTO.setDescription(template.getDescription().getDetails().getPurpose());
        }
        Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
        OpenEHRObjectBundleManager.generateTemplateObjectBundleCustomVO(templateDTO, archetypeMap);
        OpenEHRObjectBundleManager.addTemplate(templateDTO);
        Templates.loadTemplateObjectBundle(templateDTO);
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
            TemplateDTO templateDTO =
                    new TemplateDTO(idTemplate, idTemplate, idTemplate, idTemplate, null, archetypeSrc, null, null);
            Map<String, Archetype> archetypeMap = Archetypes.getArchetypeMap();
            OpenEHRObjectBundleManager.generateTemplateObjectBundleCustomVO(templateDTO, archetypeMap);
            return templateDTO;
        }catch(MissingConfigurationParameterException e){
            throw new InternalErrorException(e);
        } catch (FileNotFoundException e) {
            throw new InternalErrorException(e);
        } catch (ParseException e) {
            throw new InternalErrorException(e);
        } catch (IOException e) {
            throw new InternalErrorException(e);
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
            TerminologyDTO terminologyDTO =
                    new TerminologyDTO(terminologyId,termSetSrc);
            return terminologyDTO;
        } catch (FileNotFoundException e) {
            throw new InternalErrorException(e);
        } catch (IOException e) {
            throw new InternalErrorException(e);
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