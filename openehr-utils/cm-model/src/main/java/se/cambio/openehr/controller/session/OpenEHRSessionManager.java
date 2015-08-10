package se.cambio.openehr.controller.session;

import se.cambio.cm.model.facade.administration.delegate.CMAdministrationFacadeDelegate;
import se.cambio.cm.model.facade.terminology.delegate.TerminologyFacadeDelegate;
import se.cambio.openehr.util.BeanProvider;

public class OpenEHRSessionManager {
    private static OpenEHRSessionManager _delegate = null;

    private CMAdministrationFacadeDelegate cmAdministrationFacadeDelegate;
    private TerminologyFacadeDelegate terminologyFacadeDelegate;

    private OpenEHRSessionManager(){
    }

    public static CMAdministrationFacadeDelegate getAdministrationFacadeDelegate(){
        if (getDelegate().cmAdministrationFacadeDelegate == null) {
            getDelegate().cmAdministrationFacadeDelegate = BeanProvider.getBean(CMAdministrationFacadeDelegate.class);
        }
        return getDelegate().cmAdministrationFacadeDelegate;
    }

    public static TerminologyFacadeDelegate getTerminologyFacadeDelegate(){
        if (getDelegate().terminologyFacadeDelegate == null) {
            getDelegate().terminologyFacadeDelegate = BeanProvider.getBean(TerminologyFacadeDelegate.class);
        }
        return getDelegate().terminologyFacadeDelegate;
    }

    private static OpenEHRSessionManager getDelegate(){
        if (_delegate == null){
            _delegate = new OpenEHRSessionManager();
        }
        return _delegate;
    }
}
