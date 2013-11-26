package se.cambio.openehr.model.facade.terminology.ejb;

import org.openehr.rm.datatypes.text.CodePhrase;
import se.cambio.openehr.model.facade.terminology.delegate.TerminologyFacadeDelegate;
import se.cambio.openehr.model.facade.terminology.plain.PlainTerminologyFacadeDelegate;
import se.cambio.openehr.model.facade.terminology.vo.TerminologyNodeVO;
import se.cambio.openehr.util.exceptions.InvalidCodeException;
import se.cambio.openehr.util.exceptions.UnsupportedLanguageException;
import se.cambio.openehr.util.exceptions.UnsupportedTerminologyException;

import javax.ejb.Remote;
import javax.ejb.Stateless;
import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * User: Iago.Corbal
 * Date: 2013-11-26
 * Time: 13:39
 */

@Stateless(mappedName = "ejb/TerminologyFacadeBean")
@Remote(TerminologyFacade.class)
public class TerminologyFacadeBean implements TerminologyFacadeDelegate {

    PlainTerminologyFacadeDelegate _delegate = null;

    public TerminologyFacadeBean(){
        _delegate = new PlainTerminologyFacadeDelegate();
    }

    @Override
    public boolean isSubclassOf(CodePhrase a, CodePhrase b) throws UnsupportedTerminologyException, InvalidCodeException {
        return _delegate.isSubclassOf(a,b);
    }

    @Override
    public boolean isSubclassOf(CodePhrase a, Set<CodePhrase> b) throws UnsupportedTerminologyException, InvalidCodeException {
        return _delegate.isSubclassOf(a,b);
    }

    @Override
    public TerminologyNodeVO retrieveAllSubclasses(CodePhrase concept, CodePhrase language) throws UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException {
        return _delegate.retrieveAllSubclasses(concept, language);
    }

    @Override
    public List<TerminologyNodeVO> retrieveAll(String terminologyId, CodePhrase language) throws UnsupportedTerminologyException, UnsupportedLanguageException {
        return _delegate.retrieveAll(terminologyId, language);
    }

    @Override
    public String retrieveTerm(CodePhrase concept, CodePhrase language) throws UnsupportedTerminologyException, UnsupportedLanguageException, InvalidCodeException {
        return _delegate.retrieveTerm(concept, language);
    }

    @Override
    public boolean isValidCodePhrase(CodePhrase cp) {
        return _delegate.isValidCodePhrase(cp);
    }

    @Override
    public Collection<String> getSupportedTerminologies() {
        return _delegate.getSupportedTerminologies();
    }
}
