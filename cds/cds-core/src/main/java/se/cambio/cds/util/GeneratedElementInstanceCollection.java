package se.cambio.cds.util;

import java.util.Set;

import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.model.facade.execution.vo.GeneratedElementInstance;
import se.cambio.cds.model.instance.ArchetypeReference;

public class GeneratedElementInstanceCollection extends ElementInstanceCollection{

    public void add(ArchetypeReference archetypeReferenceToAdd){
	Set<ArchetypeReference> archetypeReferences = getArchetypeReferences(archetypeReferenceToAdd);
	if (archetypeReferences.isEmpty()){
	    archetypeReferences.add(archetypeReferenceToAdd);
	}else{
	    if (ElementInstanceCollectionUtil.isEmpty(archetypeReferenceToAdd)){
		ArchetypeReference ar = ElementInstanceCollectionUtil.getEmptyArchetypeReference(archetypeReferences);
		if (ar!=null){
		    if (!ElementInstanceCollectionUtil.containsAll(ar, archetypeReferenceToAdd)){
			for (String idElement : archetypeReferenceToAdd.getElementInstancesMap().keySet()) {
			    if (!ar.getElementInstancesMap().containsKey(idElement)){
				new GeneratedElementInstance(idElement, null, ar, null, GuideUtil.NULL_FLAVOUR_CODE_NO_INFO, null, null);
			    }
			}
		    }
		}else{
		    archetypeReferences.add(archetypeReferenceToAdd);
		}
	    }else{
		archetypeReferences.add(archetypeReferenceToAdd);
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