package se.cambio.openehr.controller.session;

import se.cambio.cm.model.facade.administration.delegate.CMAdministrationFacadeDelegate;
import se.cambio.cm.model.facade.terminology.delegate.TerminologyFacadeDelegate;

/*
    Should access all classes using autowired Spring
 */
@Deprecated
public class OpenEHRSessionManager {
    private static OpenEHRSessionManager _delegate = null;

    private CMAdministrationFacadeDelegate cmAdministrationFacadeDelegate;
    private TerminologyFacadeDelegate terminologyFacadeDelegate;

    private OpenEHRSessionManager(){
    }

    public static CMAdministrationFacadeDelegate getAdministrationFacadeDelegate(){
        return getDelegate().cmAdministrationFacadeDelegate;
    }

    public static TerminologyFacadeDelegate getTerminologyFacadeDelegate(){
        return getDelegate().terminologyFacadeDelegate;
    }

    public static void setCmAdministrationFacadeDelegate(CMAdministrationFacadeDelegate cmAdministrationFacadeDelegate) {
        getDelegate().cmAdministrationFacadeDelegate = cmAdministrationFacadeDelegate;
    }

    public static void setTerminologyFacadeDelegate(TerminologyFacadeDelegate terminologyFacadeDelegate) {
        getDelegate().terminologyFacadeDelegate = terminologyFacadeDelegate;
    }

    private static OpenEHRSessionManager getDelegate(){
        if (_delegate == null){
            _delegate = new OpenEHRSessionManager();
        }
        return _delegate;
    }
}
