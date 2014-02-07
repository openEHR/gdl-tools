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
public class DefinitionRuleLinesPanel extends RuleLinesPanel {

    public DefinitionRuleLinesPanel(GDLEditor controller) {
        super(
                controller,
                RuleLineDirectory.getSelectableDefinitions(),
                GDLEditorLanguageManager.getMessage("Definitions"));
    }

    @Override
    protected List<RuleLine> getRuleLines() {
        return getController().getDefinitionRuleLines();
    }
}
