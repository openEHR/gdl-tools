package se.cambio.openehr.util;

import java.util.Collection;

/**
 * User: iago.corbal
 * Date: 2014-01-28
 * Time: 14:25
 */
public interface TerminologyCodesManager {
    public void update();
    public void setSelectedTerminologyCodes(Collection<String> terminologyCodes);
}
