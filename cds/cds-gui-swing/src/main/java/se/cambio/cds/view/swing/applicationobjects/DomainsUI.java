package se.cambio.cds.view.swing.applicationobjects;

import javax.swing.ImageIcon;

import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.util.Domains;
import se.cambio.cds.view.swing.applicationobjects.DomainsUI;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;



public class DomainsUI {

    public static String ANY_NAME = OpenEHRLanguageManager.getMessage("ANY");
    public static String EHR_NAME = OpenEHRLanguageManager.getMessage("EHR");
    public static String CDS_NAME = OpenEHRLanguageManager.getMessage("CDS");

    private static String ANY_DESC = OpenEHRLanguageManager.getMessage("ANY"); 
    private static String EHR_DESC = OpenEHRLanguageManager.getMessage("EHR");
    private static String CDS_DESC = OpenEHRLanguageManager.getMessage("CDS"); 

    public static ImageIcon EHR_ICON = OpenEHRImageUtil.EHR_LABEL_ICON;
    public static ImageIcon CDS_ICON = OpenEHRImageUtil.CDS_LABEL_ICON;
    public static ImageIcon ANY_ICON = OpenEHRImageUtil.ANY_LABEL_ICON;

    public static ImageIcon getGroupIconFromArchetypeReference(ArchetypeReference ar){
	return getIcon(ar.getIdDomain());
    }
    
    public static ImageIcon getIcon(String idDomain){
	if (idDomain==null){
	    return DomainsUI.ANY_ICON;
	}else if (Domains.EHR_ID.equals(idDomain)){
	    return DomainsUI.EHR_ICON;
	}else if (Domains.CDS_ID.equals(idDomain)){
	    return DomainsUI.CDS_ICON;
	}
	return null;
    }

    public static String getName(String idDomain){
	if (idDomain==null){
	    return ANY_NAME;
	}else if (Domains.EHR_ID.equals(idDomain)){
	    return EHR_NAME;
	}else if (Domains.CDS_ID.equals(idDomain)){
	    return CDS_NAME;
	}
	return null;
    }

    public static String getDescription(String idDomain){
	if (idDomain==null){
	    return ANY_DESC;
	}else if (Domains.EHR_ID.equals(idDomain)){
	    return EHR_DESC;
	}else if (Domains.CDS_ID.equals(idDomain)){
	    return CDS_DESC;
	}
	return null;
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