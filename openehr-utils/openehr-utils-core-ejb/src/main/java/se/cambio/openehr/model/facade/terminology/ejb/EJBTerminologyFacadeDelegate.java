package se.cambio.openehr.model.facade.terminology.ejb;

import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.openehr.model.facade.terminology.delegate.TerminologyFacadeDelegate;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.InvalidCodeException;
import se.cambio.openehr.util.exceptions.UnsupportedLanguageException;
import se.cambio.openehr.util.exceptions.UnsupportedTerminologyException;
import se.cambio.openehr.util.util.EJBConst;
import se.cambio.openehr.util.util.OpenEHRInitialContext;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.rmi.RemoteException;
import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * User: Iago.Corbal
 * Date: 2013-11-26
 * Time: 13:42
 */
public class EJBTerminologyFacadeDelegate  implements TerminologyFacadeDelegate {


    private TerminologyFacade _delegate;

    public EJBTerminologyFacadeDelegate() throws InternalErrorException {
        try {
            InitialContext ic = OpenEHRInitialContext.getInitialContext();
            _delegate = (TerminologyFacade) ic.lookup(getLookupName());
        } catch (NamingException e) {
            throw new InternalErrorException(e);
        }
    }
    @Override
    public boolean isSubclassOf(CodePhrase a, CodePhrase b) throws InternalErrorException, UnsupportedTerminologyException, InvalidCodeException {
        try {
            return _delegate.isSubclassOf(a,b);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public boolean isSubclassOf(CodePhrase a, Set<CodePhrase> b) throws InternalErrorException, UnsupportedTerminologyException, InvalidCodeException {
        try {
            return _delegate.isSubclassOf(a,b);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language) throws InternalErrorException, UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException {
        try {
            return _delegate.retrieveAllSubclasses(concept, language);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language) throws InternalErrorException, UnsupportedTerminologyException, UnsupportedLanguageException {
        try {
            return _delegate.retrieveAll(terminologyId, language);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public String retrieveTerm(CodePhrase concept, CodePhrase language) throws InternalErrorException, UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException {
        try {
            return _delegate.retrieveTerm(concept, language);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public boolean isValidCodePhrase(CodePhrase cp) throws InternalErrorException {
        try {
            return _delegate.isValidCodePhrase(cp);
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    @Override
    public Collection<String> getSupportedTerminologies() throws InternalErrorException {
        try {
            return _delegate.getSupportedTerminologies();
        } catch (RemoteException e) {
            throw new InternalErrorException(e);
        }
    }

    private static String getLookupName() {
        String beanName = "TerminologyFacadeBean";
        final String interfaceName = TerminologyFacade.class.getName();
        return "ejb:" + EJBConst.APP_NAME + "/" + EJBConst.MODULE_NAME + "//" +
                beanName + "!" + interfaceName;
    }
}
