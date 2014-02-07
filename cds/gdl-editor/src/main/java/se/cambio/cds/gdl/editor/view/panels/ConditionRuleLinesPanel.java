package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;

import java.util.List;

/**
 * User: Iago.Corbal
 * Date: 2013-08-14
 * Time: 15:05
 */
public class ConditionRuleLinesPanel extends RuleLinesPanel {

    public ConditionRuleLinesPanel(GDLEditor controller) {
        super(controller,
                RuleLineDirectory.getSelectableConditions(),
                GDLEditorLanguageManager.getMessage("Conditions"));
    }

    @Override
    protected List<RuleLine> getRuleLines() {
        return getController().getConditionRuleLines();
    }
}
