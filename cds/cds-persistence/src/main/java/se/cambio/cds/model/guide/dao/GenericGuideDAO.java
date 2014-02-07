package se.cambio.cds.model.guide.dao;

import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Collection;
import java.util.Date;

/**
 * @author iago.corbal
 */
public interface GenericGuideDAO {

	public GuideDTO searchByGuideId(String idGuide)
	throws InternalErrorException, GuideNotFoundException;
	
	public Collection<GuideDTO> searchAll()
	throws InternalErrorException;

    public Collection<GuideDTO> searchAllDefinitions()
            throws InternalErrorException;

	public GuideDTO add(GuideDTO guideDTO)
	throws InternalErrorException;

	public void update(GuideDTO guideDTO)
	throws InternalErrorException, GuideNotFoundException;

	public void remove(String idGuide)
	throws InternalErrorException, GuideNotFoundException;

    public Date getLastUpdateDate()
            throws InternalErrorException;

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