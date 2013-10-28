package se.cambio.openehr.controller.session;

import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegate;
import se.cambio.openehr.model.facade.administration.delegate.OpenEHRAdministrationFacadeDelegateFactory;
import se.cambio.openehr.model.facade.terminology.delegate.TerminologyFacadeDelegate;
import se.cambio.openehr.model.facade.terminology.delegate.TerminologyFacadeDelegateFactory;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class OpenEHRSessionManager {
    private static OpenEHRSessionManager _delegate = null;
    private OpenEHRAdministrationFacadeDelegate _afd;
    private TerminologyFacadeDelegate _tfd = null;

    private OpenEHRSessionManager(){
    }

    public static OpenEHRAdministrationFacadeDelegate getAdministrationFacadeDelegate(){
        if (getDelegate()._afd==null){
            try {
                getDelegate()._afd = OpenEHRAdministrationFacadeDelegateFactory.getDelegate();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return getDelegate()._afd;
    }


    public static TerminologyFacadeDelegate getTerminologyFacadeDelegate(){
        if (getDelegate()._tfd==null){
            try {
                getDelegate()._tfd = TerminologyFacadeDelegateFactory.getDelegate();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return getDelegate()._tfd;
    }

    private static OpenEHRSessionManager getDelegate(){
        if (_delegate==null){
            _delegate = new OpenEHRSessionManager();
        }
        return _delegate;
    }
}
