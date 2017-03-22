package se.cambio.cds.gdl.editor.view.panels;

import se.cambio.cds.gdl.editor.controller.GDLEditor;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.editor.view.applicationobjects.RuleLineDirectory;
import se.cambio.cds.gdl.model.readable.rule.RuleLineCollection;

public class DefaultActionsPanel extends RuleLinesPanel {

    DefaultActionsPanel(GDLEditor controller) {
        super(controller,
                RuleLineDirectory.getSelectableDefaultActions(),
                GDLEditorLanguageManager.getMessage("Defaults"));
    }

    @Override
    protected RuleLineCollection getRuleLines() {
        return getController().getDefaultActions();
    }
}
