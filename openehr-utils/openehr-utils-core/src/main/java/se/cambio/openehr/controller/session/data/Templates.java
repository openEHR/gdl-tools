package se.cambio.openehr.controller.session.data;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.ImageIcon;

import se.cambio.openehr.controller.OpenEHRObjectBundleManager;
import se.cambio.openehr.controller.session.OpenEHRSessionManager;
import se.cambio.openehr.model.archetype.vo.TemplateObjectBundleCustomVO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.exceptions.InternalErrorException;


public class Templates {
    private static Templates _instance = null;
    private Map<String, TemplateDTO> _templatessById = null;
    public static ImageIcon ICON = OpenEHRImageUtil.TEMPLATE;

    private Templates(){
        _templatessById = new HashMap<String, TemplateDTO>();
    }

    public static void loadTemplates() throws InternalErrorException{
        Collection<TemplateDTO> templateDTOs =
                OpenEHRSessionManager.getAdministrationFacadeDelegate().searchAllTemplates();
        OpenEHRObjectBundleManager.generateTemplateObjectBundles(templateDTOs);
        loadTemplateObjectBundles(templateDTOs);
    }

    public static void registerTemplate(TemplateDTO templateVO){
        getDelegate()._templatessById.put(templateVO.getIdTemplate(), templateVO);
    }

    public static TemplateDTO getTemplateVO(String idTemplate){
        return getDelegate()._templatessById.get(idTemplate);
    }

    public static void loadTemplateObjectBundles(Collection<TemplateDTO> templateDTOs){
        for (TemplateDTO templateDTO : templateDTOs) {
            loadTemplateObjectBundle(templateDTO);
        }
    }

    public static void loadTemplateObjectBundle(TemplateDTO templateDTO){
        registerTemplate(templateDTO);
        TemplateObjectBundleCustomVO templateObjectBundleCustomVO =
                (TemplateObjectBundleCustomVO)IOUtils.getObject(templateDTO.getTobcVO());
        ArchetypeElements.loadArchetypeElements(templateObjectBundleCustomVO.getElementVOs());
        Clusters.loadClusters(templateObjectBundleCustomVO.getClusterVOs());
        CodedTexts.loadCodedTexts(templateObjectBundleCustomVO.getCodedTextVOs());
        Ordinals.loadOrdinals(templateObjectBundleCustomVO.getOrdinalVOs());
        //ArchetypeSlots.loadArchetypeNodes(templateObjectBundleCustomVO.getSlotVOs());
        Units.loadUnits(templateObjectBundleCustomVO.getUnitVOs());
    }

    public static ArrayList<TemplateDTO> getTemplates(String entryType){
        ArrayList<TemplateDTO> list = new ArrayList<TemplateDTO>();
        for (TemplateDTO templateVO : getDelegate()._templatessById.values()) {
            if (entryType.equals(templateVO.getRMName())){
                list.add(templateVO);
            }
        }
        return list;
    }

    public static ImageIcon getIcon(String idTemplate){
        String entryType = getTemplateVO(idTemplate).getRMName();
        ImageIcon icon = OpenEHRConstUI.getIcon(entryType);
        if (icon!=null){
            return icon;
        }else{
            return ICON;
        }
    }

    public static Collection<TemplateDTO> getAllTemplates(){
        return new ArrayList<TemplateDTO>(getDelegate()._templatessById.values());
    }

    public static Collection<String> getAllTemplateIds(){
        return new ArrayList<String>(getDelegate()._templatessById.keySet());
    }

    public static Templates getDelegate(){
        if (_instance == null){
            _instance = new Templates();
        }
        return _instance;
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