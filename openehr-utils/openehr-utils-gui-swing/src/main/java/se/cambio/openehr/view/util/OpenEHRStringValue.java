package se.cambio.openehr.view.util;

import org.apache.log4j.Logger;
import org.jdesktop.swingx.renderer.StringValue;
import se.cambio.openehr.model.archetype.vo.PathableVO;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 13:07
 */
public class OpenEHRStringValue implements StringValue {

    @Override
    public String getString(Object value) {
        if (value instanceof PathableVO){
            return ((PathableVO)value).getName();
        }else{
            Logger.getLogger(OpenEHRStringValue.class).warn("Unknown element in table!");
            return null;
        }
    }
}