package se.cambio.cds.gdl.model.readable.rule;

import se.cambio.cds.gdl.model.readable.ReadableGuide;
import se.cambio.cds.gdl.model.readable.rule.lines.RuleLine;
import se.cambio.openehr.controller.session.data.ArchetypeManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class RuleLineCollection {
    private ReadableGuide readableGuide;
    private List<RuleLine> ruleLines;


    public RuleLineCollection(ReadableGuide readableGuide) {
        this.readableGuide = readableGuide;
    }

    public List<RuleLine> getRuleLines() {
        return Collections.unmodifiableList(getRuleLinesI());
    }

    private List<RuleLine> getRuleLinesI() {
        if (ruleLines == null) {
            ruleLines = new ArrayList<RuleLine>();
        }
        return ruleLines;
    }

    public void add(RuleLine ruleLine){
        getRuleLinesI().add(ruleLine);
        ruleLine.setReadableRule(readableGuide);
    }

    public void remove(RuleLine ruleLine){
        getRuleLinesI().remove(ruleLine);
        ruleLine.setReadableRule(null);
    }

    public ArchetypeManager getArchetypeManager() {
        return readableGuide.getArchetypeManager();
    }
}
