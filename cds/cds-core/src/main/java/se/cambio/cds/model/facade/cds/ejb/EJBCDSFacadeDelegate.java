/*
 * Created on 02-jun-2006
 *


 */
package se.cambio.cds.model.facade.cds.ejb;


import java.rmi.RemoteException;
import java.util.Calendar;
import java.util.Collection;

import se.cambio.cds.controller.guide.ElementInstanceCollection;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.model.facade.cds.delegate.CDSFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.ElementInstance;
import se.cambio.cds.model.facade.execution.vo.ExecutionMode;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.util.exceptions.GuideNotFoundException;
import se.cambio.cds.util.exceptions.InternalErrorException;
import se.cambio.cds.util.exceptions.PatientNotFoundException;
/**
 * @author icorram
 *
 */
public class EJBCDSFacadeDelegate implements CDSFacadeDelegate {

    private CDSFacade cdsFacade;    
    /*
    private final static String CDS_FACADE_HOME_JNDI_NAME_PARAMETER = "EJBCDSFacadeDelegate/cdsFacadeHomeJNDIName";

   private static String cdsFacadeHomeJNDIName;


    static {
	try {
	    //Initialize "cdsFacadeHomeJNDIName".
	    cdsFacadeHomeJNDIName = ConfigurationParametersManager
		    .getParameter(CDS_FACADE_HOME_JNDI_NAME_PARAMETER);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
     */

    public EJBCDSFacadeDelegate() throws InternalErrorException {
	/*
	CDSFacadeHome cdsFacadeHome;
	try {
	    cdsFacadeHome = (CDSFacadeHome) 
		    EJBHomeLocator.getEJBHome(
			    cdsFacadeHomeJNDIName,
			    CDSFacadeHome.class);
	    cdsFacade = cdsFacadeHome.create();
	} catch (NamingException e) {
	    throw new InternalErrorException(e);
	} catch (RemoteException e) {
	    throw new InternalErrorException(e);
	} catch (CreateException e) {
	    throw new InternalErrorException(e);
	}*/
    }


    public RuleExecutionResult monitor(String idPatient, ElementInstanceCollection elementInstanceCollection) 
	    throws InternalErrorException, PatientNotFoundException{
	try{
	    return cdsFacade.monitor(idPatient, elementInstanceCollection);
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }

    public RuleExecutionResult monitor(
	    String idPatient, 
	    ElementInstanceCollection elementInstanceCollection,
	    Calendar date, 
	    ExecutionMode executionMode) 
		    throws InternalErrorException, PatientNotFoundException{
	try{
	    return cdsFacade.monitor(idPatient, elementInstanceCollection, date, executionMode);
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }

    public Collection<ElementInstance> getEHRElements(String idGuide)
	    throws InternalErrorException, GuideNotFoundException{
	try{
	    return cdsFacade.getEHRElements(idGuide);
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }

    public Collection<ElementInstance> getAllEHRElements()
	    throws InternalErrorException{
	try{
	    return cdsFacade.getAllEHRElements();
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }

    public Collection<ElementInstance> getElementInstances(String idGuide)
	    throws InternalErrorException, GuideNotFoundException{
	try{
	    return cdsFacade.getElements(idGuide);
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }

    public Collection<ElementInstance> getAllElements()
	    throws InternalErrorException{
	try{
	    return cdsFacade.getAllElements();
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }


    public RuleExecutionResult monitor(String idPatient,
	    Collection<GuideDTO> guides,
	    ElementInstanceCollection elementInstanceCollection, Calendar date)
		    throws InternalErrorException, PatientNotFoundException {
	try{
	    return cdsFacade.monitor(idPatient, guides, elementInstanceCollection, date);
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }


    public GuideManager getGuideManager() throws InternalErrorException {
	try{
	    return cdsFacade.getGuideManager();
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }


    public void setGuideManager(GuideManager guideManager)
	    throws InternalErrorException {
	try{
	    cdsFacade.setGuideManager(guideManager);
	}catch(RemoteException e){
	    throw new InternalErrorException(e);
	}
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  1.1 (the 'License'); you may not use this file except in compliance with
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