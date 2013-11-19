package se.cambio.openehr.view.util;

import org.openehr.rm.datatypes.basic.DataValue;
import se.cambio.openehr.controller.session.data.ArchetypeElements;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.model.archetype.vo.ArchetypeElementVO;
import se.cambio.openehr.util.DataValuesGroupVO;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.view.panels.TemplateListTableFormPanel;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2013-11-11
 * Time: 11:21
 */
public class TestTemplateListTableForm {
    public static void main(String[] args){
        try {
            Archetypes.loadArchetypes();
            Templates.loadTemplates();
        } catch (InternalErrorException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }


        //Map<String, DataValue> dataValueMap = new HashMap<String, DataValue>();
        String path ="/";
        List<DataValuesGroupVO> dataValuesGroupVOList = new ArrayList<DataValuesGroupVO>();
        dataValuesGroupVOList.add(new DataValuesGroupVO(new HashMap <String, DataValue>(), path));
        dataValuesGroupVOList.add(new DataValuesGroupVO(new HashMap <String, DataValue>(), path));
        dataValuesGroupVOList.add(new DataValuesGroupVO(new HashMap <String, DataValue>(), path));
        dataValuesGroupVOList.add(new DataValuesGroupVO(new HashMap <String, DataValue>(), path));
        //dataValueMap.put("openEHR-EHR-INSTRUCTION.medication.v1/activities[at0001]/description[openEHR-EHR-ITEM_TREE.medication.v1]/items[at0003]", new DvText("Test value"));
        String archetypeId = "openEHR-EHR-COMPOSITION.encounter.v1";
        String templateId = "test_template";
        List<ArchetypeElementVO> archetypeElementVOs =
                new ArrayList<ArchetypeElementVO>(ArchetypeElements.getArchetypeElementsVO(archetypeId, templateId));
        TemplateListTableFormPanel panel =
                new TemplateListTableFormPanel(templateId, "/",archetypeElementVOs, dataValuesGroupVOList);
        Frame frame = new Frame();
        JDialog dialog = new JDialog(frame,"test", true);
        dialog.setSize(new Dimension(640,480));
        dialog.setContentPane(panel);
        dialog.setVisible(true);
        System.exit(0);
    }
}
