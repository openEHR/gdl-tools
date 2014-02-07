package se.cambio.cds.controller;

import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegate;
import se.cambio.cds.model.facade.administration.delegate.CDSAdministrationFacadeDelegateFactory;
import se.cambio.cds.model.facade.cds.delegate.CDSExecutionFacadeDelegate;
import se.cambio.cds.model.facade.cds.delegate.CDSExecutionFacadeDelegateFactory;
import se.cambio.cds.model.facade.ehr.delegate.EHRFacadeDelegate;
import se.cambio.cds.model.facade.ehr.delegate.EHRFacadeDelegateFactory;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegate;
import se.cambio.cds.model.facade.execution.delegate.RuleExecutionFacadeDelegateFactory;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class CDSSessionManager {
    private static CDSSessionManager _delegate = null;

    private CDSAdministrationFacadeDelegate _afd = null;
    private CDSExecutionFacadeDelegate _cdsfd = null;
    private RuleExecutionFacadeDelegate _refd = null;
    private EHRFacadeDelegate _ehrfd;

    private CDSSessionManager(){
    }


    public static CDSAdministrationFacadeDelegate getAdministrationFacadeDelegate(){
        if (getDelegate()._afd==null){
            try {
                getDelegate()._afd = CDSAdministrationFacadeDelegateFactory.getDelegate();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return getDelegate()._afd;
    }

    public static CDSExecutionFacadeDelegate getCDSExecutionFacadeDelegate(){
        if (getDelegate()._cdsfd==null){
            try {
                getDelegate()._cdsfd = CDSExecutionFacadeDelegateFactory.getDelegate();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return getDelegate()._cdsfd;
    }

    public static RuleExecutionFacadeDelegate getRuleExecutionFacadeDelegate(){
        if (getDelegate()._refd==null){
            try {
                getDelegate()._refd = RuleExecutionFacadeDelegateFactory.getDelegate();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return getDelegate()._refd;
    }

    public static EHRFacadeDelegate getEHRFacadeDelegate(){
        if (getDelegate()._ehrfd==null){
            try {
                getDelegate()._ehrfd = EHRFacadeDelegateFactory.getDelegate();
            } catch (InternalErrorException e) {
                ExceptionHandler.handle(e);
            }
        }
        return getDelegate()._ehrfd;
    }

    private static CDSSessionManager getDelegate(){
        if (_delegate==null){
            _delegate = new CDSSessionManager();
        }
        return _delegate;
    }
}
