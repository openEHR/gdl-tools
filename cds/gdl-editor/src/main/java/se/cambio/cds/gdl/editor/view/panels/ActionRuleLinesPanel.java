package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;

public class ActionRuleLinesPanel extends RuleLinesPanel {

    ActionRuleLinesPanel(GDLEditor controller) {
        super(
                controller,
                RuleLineDirectory.getSelectableActions(),
                GDLEditorLanguageManager.getMessage("Actions"));
    }

    @Override
    protected RuleLineCollection getRuleLines() {
        return getController().getActionsRuleLines();
    }
}
