package se.cambio.cds.gdl.editor.controller;

import difflib.Delta;
import difflib.DiffUtils;
import difflib.Patch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.cds.controller.guide.GuideUtil;
import se.cambio.cds.gdl.editor.util.GDLEditorLanguageManager;
import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.gdl.model.Term;
import se.cambio.cds.gdl.model.TermDefinition;
import se.cambio.openehr.view.dialogs.DialogLongMessageNotice;
import se.cambio.openehr.view.util.WindowManager;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class GuidelineEditorManager {

    private Logger logger = LoggerFactory.getLogger(GuidelineEditorManager.class);
    private WindowManager windowManager;

    public GuidelineEditorManager(WindowManager windowManager) {
        this.windowManager = windowManager;
    }

    String serializeGuide(Guide guide) {
        try {
            return GuideUtil.serializeGuide(guide);
        } catch (Exception ex) {
            ex.printStackTrace();
            DialogLongMessageNotice dialog = new DialogLongMessageNotice(
                    windowManager.getMainWindow(),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuideT"),
                    GDLEditorLanguageManager.getMessage("ErrorSerializingGuide"),
                    ex.getMessage(), DialogLongMessageNotice.MessageType.ERROR);
            dialog.setVisible(true);
            return null;
        }
    }

    public Guide parseGuide(InputStream input) {
        try {
            return GuideUtil.parseGuide(input);
        } catch (Exception ex) {
            ex.printStackTrace();
            DialogLongMessageNotice dialog = new DialogLongMessageNotice(
                    windowManager.getMainWindow(),
                    GDLEditorLanguageManager.getMessage("ErrorParsingGuideT"),
                    GDLEditorLanguageManager.getMessage("ErrorParsingGuide"),
                    ex.getMessage(), DialogLongMessageNotice.MessageType.ERROR);
            dialog.setVisible(true);
            return null;
        }
    }

    void check(String originalLang, Map<String, TermDefinition> termDefinitionMap) {
        TermDefinition originalTermDefinition =
                termDefinitionMap.get(originalLang);
        for (Map.Entry<String, TermDefinition> langCode : termDefinitionMap.entrySet()) {
            if (!langCode.getKey().equals(originalLang)) {
                TermDefinition td = langCode.getValue();
                for (String gtCode : originalTermDefinition.getTerms().keySet()) {
                    if (!td.getTerms().containsKey(gtCode)) {
                        logger.warn(
                                "Language '" + langCode
                                        + "' does not contain gt code '"
                                        + gtCode + "'. Will be added...");
                        Term term = getTermToDifferentLanguage(gtCode,
                                originalTermDefinition.getTerms().get(gtCode),
                                originalLang);
                        td.getTerms().put(gtCode, term);
                    }
                }
            }
        }
    }

    public boolean checkParsedGuide(String guideSrc, Guide guide) {
        String guideSrcAux = serializeGuide(guide);
        if (guide != null) {
            Patch patch = DiffUtils.diff(stringToLines(guideSrc), stringToLines(guideSrcAux));
            if (patch.getDeltas().isEmpty()) {
                return true;
            } else {
                StringBuilder diff = new StringBuilder();
                for (Delta delta : patch.getDeltas()) {
                    diff.append("-------------------------------------\n");
                    diff.append(" line:").append(delta.getOriginal().getPosition()).append(1).append("\n");
                    diff.append(" original:").append(delta.getOriginal().getLines()).append("\n");
                    diff.append(" revised:").append(delta.getRevised().getLines()).append("\n");
                }
                DialogLongMessageNotice dialog =
                        new DialogLongMessageNotice(
                                windowManager.getMainWindow(),
                                GDLEditorLanguageManager.getMessage("ErrorLoadingGuideT"),
                                GDLEditorLanguageManager.getMessage("ErrorLoadingGuide"),
                                diff.toString(),
                                DialogLongMessageNotice.MessageType.WARNING_WITH_CANCEL
                        );
                dialog.setVisible(true);
                return dialog.getAnswer();
            }
        } else {
            return false;
        }
    }

    static Term getTermToDifferentLanguage(String gtCode, Term originalTerm, String originalLanguage) {
        Term newTerm = new Term();
        newTerm.setId(gtCode);
        String text = null;
        String description = null;
        String languagePrefix = "*(" + originalLanguage + ") ";
        if (originalTerm != null && originalTerm.getText() != null
                && !originalTerm.getText().isEmpty()) {
            text = languagePrefix + originalTerm.getText();
        }
        if (originalTerm != null && originalTerm.getDescription() != null
                && !originalTerm.getDescription().isEmpty()) {
            description = languagePrefix + originalTerm.getDescription();
        }
        newTerm.setText(text);
        newTerm.setDescription(description);
        return newTerm;
    }

    private static List<String> stringToLines(String str) {
        final List<String> lines = new ArrayList<>();
        for (String string : str.split("\n")) {
            lines.add(string.trim());
        }
        return lines;
    }

}
