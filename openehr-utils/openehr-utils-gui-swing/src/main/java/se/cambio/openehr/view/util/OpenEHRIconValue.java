package se.cambio.openehr.view.util;

import org.apache.log4j.Logger;
import org.jdesktop.swingx.renderer.IconValue;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.model.archetype.vo.ClusterVO;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.OpenEHRDataValuesUI;

import javax.swing.*;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 13:07
 */
public class OpenEHRIconValue implements IconValue {

    public Icon getIcon(Object value) {
        if (value instanceof ArchetypeElementVO){
            return OpenEHRDataValuesUI.getIcon(((ArchetypeElementVO)value).getRMType());
        }else if (value instanceof ClusterVO){
                return OpenEHRConstUI.getIcon(((ClusterVO)value).getRMType());
        }else{
            Logger.getLogger(OpenEHRIconValue.class).warn("Unknown element in table!");
            return null;
        }
    }
}