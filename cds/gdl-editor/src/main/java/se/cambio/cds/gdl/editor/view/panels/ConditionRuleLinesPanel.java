package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;

public class ConditionRuleLinesPanel extends RuleLinesPanel {

    ConditionRuleLinesPanel(GDLEditor controller) {
        super(controller,
                RuleLineDirectory.getSelectableConditions(),
                GDLEditorLanguageManager.getMessage("Conditions"));
    }

    @Override
    protected RuleLineCollection getRuleLines() {
        return getController().getConditionRuleLines();
    }
}
