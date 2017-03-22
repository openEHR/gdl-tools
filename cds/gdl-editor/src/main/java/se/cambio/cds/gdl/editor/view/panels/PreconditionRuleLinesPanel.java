package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;

public class PreconditionRuleLinesPanel extends RuleLinesPanel {

    PreconditionRuleLinesPanel(GDLEditor controller) {
        super(controller,
                RuleLineDirectory.getSelectablePreconditions(),
                GDLEditorLanguageManager.getMessage("Preconditions"));
    }

    @Override
    protected RuleLineCollection getRuleLines() {
        return getController().getPreconditionRuleLines();
    }
}
