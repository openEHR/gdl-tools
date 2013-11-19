package se.cambio.openehr.view.listeners;

import se.cambio.openehr.util.DataValuesGroupVO;

/**
 * User: Iago.Corbal
 * Date: 2013-11-15
 * Time: 15:46
 */
public interface TemplateTableListener {

    public void dataValuesGroupAdded(DataValuesGroupVO dataValuesGroupVO);
    public void dataValuesGroupRemoved(DataValuesGroupVO dataValuesGroupVO);
    public void dataValueChanged(DataValuesGroupVO dataValuesGroupVO, String elementId);
}
