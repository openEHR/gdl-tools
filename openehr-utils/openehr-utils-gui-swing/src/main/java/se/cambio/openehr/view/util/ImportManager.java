package se.cambio.openehr.view.util;

import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.generic.dao.FileGenericCMElementDAO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.util.CmFolder;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.File;
import java.util.Collection;
import java.util.Collections;

public class ImportManager {

    private ArchetypeManager archetypeManager;

    public ImportManager(ArchetypeManager archetypeManager) {
        this.archetypeManager = archetypeManager;
    }

    public int showImportArchetypeDialogAndAddToRepo(Window owner, File selectedFile) throws InternalErrorException, InstanceNotFoundException {
        JFileChooser fileChooser = getArchetypeFileChooser(selectedFile);
        int result = fileChooser.showOpenDialog(owner);
        if (result != JFileChooser.CANCEL_OPTION) {
            addArchetype(fileChooser.getSelectedFile());
        }
        return result;
    }

    private static JFileChooser getArchetypeFileChooser(File selectedFile) {
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter(
                OpenEHRLanguageManager.getMessage("Archetype"), "adl");
        fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ImportArchetype"));
        fileChooser.setFileFilter(filter);
        if (selectedFile != null) {
            fileChooser.setSelectedFile(selectedFile);
        }
        return fileChooser;
    }

    public int showImportTemplateDialog(Window owner, File selectedFile) throws InternalErrorException, InstanceNotFoundException {
        JFileChooser fileChooser = getTemplateFileChooser(selectedFile);
        int result = fileChooser.showOpenDialog(owner);
        if (result != JFileChooser.CANCEL_OPTION) {
            addTemplate(fileChooser.getSelectedFile());
        }
        return result;
    }

    private JFileChooser getTemplateFileChooser(File selectedFile) {
        JFileChooser fileChooser = new JFileChooser();
        FileNameExtensionFilter filter = new FileNameExtensionFilter(
                OpenEHRLanguageManager.getMessage("Template"), "oet");
        fileChooser.setDialogTitle(OpenEHRLanguageManager.getMessage("ImportTemplate"));
        fileChooser.setFileFilter(filter);
        if (selectedFile != null) {
            fileChooser.setSelectedFile(selectedFile);
        }
        return fileChooser;
    }

    private void addArchetype(File file) throws InstanceNotFoundException, InternalErrorException {
        ArchetypeDTO archetypeDTO = getArchetypeDTOFromFile(file);
        archetypeManager.getArchetypes().upsert(archetypeDTO);
    }

    private ArchetypeDTO getArchetypeDTOFromFile(File file) throws InternalErrorException, InstanceNotFoundException {
        File folder = file.getParentFile();
        CmFolder cmFolder = new CmFolder(folder);
        FileGenericCMElementDAO<ArchetypeDTO> dao =
                new FileGenericCMElementDAO<>(ArchetypeDTO.class, cmFolder);
        String fileName = file.getName();
        String archetypeId = fileName.substring(0, fileName.length() - ".adl".length());
        Collection<ArchetypeDTO> archetypeDTOs = dao.searchByIds(Collections.singleton(archetypeId));
        return archetypeDTOs.iterator().next();
    }

    private void addTemplate(File file) throws InstanceNotFoundException, InternalErrorException {
        TemplateDTO templateDTO = getTemplateDTOFromFile(file);
        archetypeManager.getTemplates().upsert(templateDTO);
    }


    private static TemplateDTO getTemplateDTOFromFile(File file) throws InternalErrorException, InstanceNotFoundException {
        File folder = file.getParentFile();
        CmFolder cmFolder = new CmFolder(folder);
        FileGenericCMElementDAO<TemplateDTO> dao =
                new FileGenericCMElementDAO<>(TemplateDTO.class, cmFolder);
        String fileName = file.getName();
        String templateId = fileName.substring(0, fileName.length() - ".oet".length());
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