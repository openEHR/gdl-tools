package se.cambio.cds.gdl.editor.util;

import se.cambio.openehr.controller.InitialLoadingObservable;
import se.cambio.openehr.controller.InitialLoadingObservable.LoadingStage;

public class GDLLoadingUtility {
    public static String getCurrentLoadingStageName() {
        LoadingStage currentLoadingStage = InitialLoadingObservable.getCurrentLoadingStage();
        if (LoadingStage.ARCHETYPES.equals(currentLoadingStage)) {
            return GDLEditorLanguageManager.getMessage("LoadingArchetypes") + "...";
        } else if (LoadingStage.TEMPLATES.equals(currentLoadingStage)) {
            return GDLEditorLanguageManager.getMessage("LoadingTemplates") + "...";
        } else if (LoadingStage.TERMINOLOGIES.equals(currentLoadingStage)) {
            return GDLEditorLanguageManager.getMessage("LoadingTerminologies") + "...";
        } else if (LoadingStage.ONTOLOGIES.equals(currentLoadingStage)) {
            return GDLEditorLanguageManager.getMessage("LoadingOntologies") + "...";
        } else if (LoadingStage.GUIDES.equals(currentLoadingStage)) {
            return GDLEditorLanguageManager.getMessage("LoadingGuidelines") + "...";
        } else {
            return "";
        }
    }
}
