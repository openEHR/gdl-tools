package se.cambio.openehr.util;

/**
 * User: Iago.Corbal
 * Date: 2013-11-15
 * Time: 19:39
 */
public interface TemplateTableChangeManager {

    public void notifyChange(DataValuesGroupVO dataValuesGroupVO, String elementId);
}
