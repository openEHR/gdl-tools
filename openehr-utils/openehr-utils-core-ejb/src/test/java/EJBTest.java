import se.cambio.openehr.model.facade.administration.ejb.EJBOpenEHRAdministrationFacadeDelegate;
import se.cambio.openehr.util.exceptions.InternalErrorException;

/**
 * User: Iago.Corbal
 * Date: 2013-10-30
 * Time: 10:12
 */
public class EJBTest {
    public static void main(String[] args){
        try {
            EJBOpenEHRAdministrationFacadeDelegate ejb = new EJBOpenEHRAdministrationFacadeDelegate();
            System.out.println(ejb.searchAllArchetypesDefinitions().size());
        } catch (InternalErrorException e) {
            e.printStackTrace();
        }

    }
}
