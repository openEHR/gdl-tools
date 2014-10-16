package se.cambio.openehr.view.util;

import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.cm.element.dao.FileGenericCMElementDAO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.File;
import java.util.Collection;
import java.util.Collections;

public class ImportUtils {

    public static int showImportArchetypeDialogAndAddToRepo(Window owner, File selectedFile) throws InternalErrorException, InstanceNotFoundException {
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

    public static int showImportTemplateDialog(Window owner, File selectedFile) throws InternalErrorException, InstanceNotFoundException {
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
    private static void addArchetype(final Window owner, File file) throws InstanceNotFoundException, InternalErrorException {
        ArchetypeDTO archetypeDTO = getArchetypeDTOFromFile(file);
        Archetypes.getInstance().upsert(archetypeDTO);
    }

    public static ArchetypeDTO getArchetypeDTOFromFile(File file) throws InternalErrorException, InstanceNotFoundException {
        FileGenericCMElementDAO<ArchetypeDTO> dao =
                new FileGenericCMElementDAO<ArchetypeDTO>(ArchetypeDTO.class, file.getParentFile(), Collections.singleton("adl"));
        String fileName = file.getName();
        String archetypeId = fileName.substring(0, fileName.length()-".adl".length());
        Collection<ArchetypeDTO> archetypeDTOs = dao.searchByIds(Collections.singleton(archetypeId));
        return archetypeDTOs.iterator().next();
    }

    private static void addTemplate(final Window owner, File file) throws InstanceNotFoundException, InternalErrorException {
        TemplateDTO templateDTO = getTemplateDTOFromFile(file);
        Templates.getInstance().upsert(templateDTO);
    }


    public static TemplateDTO getTemplateDTOFromFile(File file) throws InternalErrorException, InstanceNotFoundException {
        FileGenericCMElementDAO<TemplateDTO> dao =
                new FileGenericCMElementDAO<TemplateDTO>(TemplateDTO.class, file.getParentFile(), Collections.singleton("adl"));
        String fileName = file.getName();
        String templateId = fileName.substring(0, fileName.length()-".adl".length());
        Collection<TemplateDTO> templateDTOs = dao.searchByIds(Collections.singleton(templateId));
        return templateDTOs.iterator().next();
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