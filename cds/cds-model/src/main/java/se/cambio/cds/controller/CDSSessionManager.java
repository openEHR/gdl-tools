package se.cambio.cds.controller;

import se.cambio.cds.model.facade.ehr.delegate.EHRFacadeDelegate;
import se.cambio.cds.model.facade.execution.delegate.RuleEngineFacadeDelegate;
import se.cambio.openehr.util.BeanProvider;

public class CDSSessionManager {
    private static CDSSessionManager instance;
    private RuleEngineFacadeDelegate refd;
    private EHRFacadeDelegate ehrfd;

    private CDSSessionManager() {
    }

    public static RuleEngineFacadeDelegate getRuleEngineFacadeDelegate() {
        if (getDelegate().refd == null) {
            getDelegate().refd = BeanProvider.getBean(RuleEngineFacadeDelegate.class);
        }
        return getDelegate().refd;
    }

    public static EHRFacadeDelegate getEHRFacadeDelegate() {
        if (getDelegate().ehrfd == null) {
            getDelegate().ehrfd = BeanProvider.getBean(EHRFacadeDelegate.class);
        }
        return getDelegate().ehrfd;
    }

    private static CDSSessionManager getDelegate() {
        if (instance == null) {
            instance = new CDSSessionManager();
        }
        return instance;
    }
}
