package se.cambio.openehr.util;

import org.openehr.rm.datatypes.text.CodePhrase;
import org.openehr.rm.datatypes.text.DvCodedText;
import org.openehr.rm.support.terminology.TerminologyService;

import javax.swing.*;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

public class OpenEHRConstUI {

    private static OpenEHRConstUI _instance = null;

    public static final DvCodedText NULL_FLAVOUR_CODE_NO_INFO = new DvCodedText("no information", new CodePhrase(TerminologyService.OPENEHR, "271"));
    public static final DvCodedText NULL_FLAVOUR_CODE_UNKNOWN = new DvCodedText("Unknown", new CodePhrase(TerminologyService.OPENEHR, "253"));
    public static final DvCodedText NULL_FLAVOUR_CODE_MASKED = new DvCodedText("Masked", new CodePhrase(TerminologyService.OPENEHR, "272"));
    public static final DvCodedText NULL_FLAVOUR_CODE_NOT_APPLICABLE = new DvCodedText("Not applicable", new CodePhrase(TerminologyService.OPENEHR, "273"));


    private final HashMap<String, String> openEHRConstNames;
    private final HashMap<String, String> openEHRConstDescriptions;
    private final HashMap<String, ImageIcon> openEHRConstIcons;
    private final HashMap<String, String> openEHRConstIconNames;

    public static final Map<String, DvCodedText> NULL_FLAVOUR_MAP = new LinkedHashMap<>();

    static {
        NULL_FLAVOUR_MAP.put("271", NULL_FLAVOUR_CODE_NO_INFO);
        NULL_FLAVOUR_MAP.put("253", NULL_FLAVOUR_CODE_UNKNOWN);
        NULL_FLAVOUR_MAP.put("272", NULL_FLAVOUR_CODE_MASKED);
        NULL_FLAVOUR_MAP.put("273", NULL_FLAVOUR_CODE_NOT_APPLICABLE);
    }

    private OpenEHRConstUI() {
        openEHRConstNames = new HashMap<>();
        openEHRConstDescriptions = new HashMap<>();
        openEHRConstIcons = new HashMap<>();
        openEHRConstIconNames = new HashMap<>();

        openEHRConstNames.put(OpenEHRConst.HISTORY, OpenEHRLanguageManager.getMessage("History"));
        openEHRConstDescriptions.put(OpenEHRConst.HISTORY, OpenEHRLanguageManager.getMessage("HistoryDesc"));
        openEHRConstIcons.put(OpenEHRConst.HISTORY, OpenEHRImageUtil.STRUCTURE);    //TODO
        openEHRConstIconNames.put(OpenEHRConst.HISTORY, OpenEHRImageUtil.STRUCTURE_NAME);  //TODO

        openEHRConstNames.put(OpenEHRConst.EVENT, OpenEHRLanguageManager.getMessage("Event"));
        openEHRConstDescriptions.put(OpenEHRConst.EVENT, OpenEHRLanguageManager.getMessage("EventDesc"));
        openEHRConstIcons.put(OpenEHRConst.EVENT, OpenEHRImageUtil.EVENT);
        openEHRConstIconNames.put(OpenEHRConst.EVENT, OpenEHRImageUtil.EVENT_NAME);

        openEHRConstNames.put(OpenEHRConst.CLUSTER, OpenEHRLanguageManager.getMessage("Cluster"));
        openEHRConstDescriptions.put(OpenEHRConst.CLUSTER, OpenEHRLanguageManager.getMessage("ClusterDesc"));
        openEHRConstIcons.put(OpenEHRConst.CLUSTER, OpenEHRImageUtil.CLUSTER);
        openEHRConstIconNames.put(OpenEHRConst.CLUSTER, OpenEHRImageUtil.CLUSTER_NAME);

        openEHRConstNames.put(OpenEHRConst.SECTION, OpenEHRLanguageManager.getMessage("Section"));
        openEHRConstDescriptions.put(OpenEHRConst.SECTION, OpenEHRLanguageManager.getMessage("SectionDesc"));
        openEHRConstIcons.put(OpenEHRConst.SECTION, OpenEHRImageUtil.SECTION);
        openEHRConstIconNames.put(OpenEHRConst.SECTION, OpenEHRImageUtil.SECTION_NAME);

        openEHRConstNames.put(OpenEHRConst.ACTIVITY, OpenEHRLanguageManager.getMessage("Activity"));
        openEHRConstDescriptions.put(OpenEHRConst.ACTIVITY, OpenEHRLanguageManager.getMessage("ActivityDesc"));
        openEHRConstIcons.put(OpenEHRConst.ACTIVITY, OpenEHRImageUtil.ACTIVITY);
        openEHRConstIconNames.put(OpenEHRConst.ACTIVITY, OpenEHRImageUtil.ACTIVITY_NAME);

        openEHRConstNames.put(OpenEHRConst.STRUCTURE, OpenEHRLanguageManager.getMessage("Structure"));
        openEHRConstDescriptions.put(OpenEHRConst.STRUCTURE, OpenEHRLanguageManager.getMessage("StructureDesc"));
        openEHRConstIcons.put(OpenEHRConst.STRUCTURE, OpenEHRImageUtil.STRUCTURE);
        openEHRConstIconNames.put(OpenEHRConst.STRUCTURE, OpenEHRImageUtil.STRUCTURE_NAME);

        openEHRConstNames.put(OpenEHRConst.ITEM_TREE, OpenEHRLanguageManager.getMessage("ItemTree"));
        openEHRConstDescriptions.put(OpenEHRConst.ITEM_TREE, OpenEHRLanguageManager.getMessage("ItemTreeDesc"));
        openEHRConstIcons.put(OpenEHRConst.ITEM_TREE, OpenEHRImageUtil.ITEM_TREE);
        openEHRConstIconNames.put(OpenEHRConst.ITEM_TREE, OpenEHRImageUtil.ITEM_TREE_NAME);

        openEHRConstNames.put(OpenEHRConst.ITEM_LIST, OpenEHRLanguageManager.getMessage("ItemList"));
        openEHRConstDescriptions.put(OpenEHRConst.ITEM_LIST, OpenEHRLanguageManager.getMessage("ItemListDesc"));
        openEHRConstIcons.put(OpenEHRConst.ITEM_LIST, OpenEHRImageUtil.ITEM_LIST);
        openEHRConstIconNames.put(OpenEHRConst.ITEM_LIST, OpenEHRImageUtil.ITEM_LIST_NAME);

        openEHRConstNames.put(OpenEHRConst.ITEM_TABLE, OpenEHRLanguageManager.getMessage("ItemTable"));
        openEHRConstDescriptions.put(OpenEHRConst.ITEM_TABLE, OpenEHRLanguageManager.getMessage("ItemTableDesc"));
        openEHRConstIcons.put(OpenEHRConst.ITEM_TABLE, OpenEHRImageUtil.ITEM_TABLE);
        openEHRConstIconNames.put(OpenEHRConst.ITEM_TABLE, OpenEHRImageUtil.ITEM_TABLE_NAME);

        openEHRConstNames.put(OpenEHRConst.ITEM_SINGLE, OpenEHRLanguageManager.getMessage("ItemSingle"));
        openEHRConstDescriptions.put(OpenEHRConst.ITEM_SINGLE, OpenEHRLanguageManager.getMessage("ItemSingleDesc"));
        openEHRConstIcons.put(OpenEHRConst.ITEM_SINGLE, OpenEHRImageUtil.ITEM_SINGLE);
        openEHRConstIconNames.put(OpenEHRConst.ITEM_SINGLE, OpenEHRImageUtil.ITEM_SINGLE_NAME);


        openEHRConstNames.put(OpenEHRConst.ELEMENT, OpenEHRLanguageManager.getMessage("Element"));
        openEHRConstDescriptions.put(OpenEHRConst.ELEMENT, OpenEHRLanguageManager.getMessage("ElementDesc"));
        openEHRConstIcons.put(OpenEHRConst.ELEMENT, OpenEHRImageUtil.ELEMENT);
        openEHRConstIconNames.put(OpenEHRConst.ELEMENT, OpenEHRImageUtil.ELEMENT_NAME);

        openEHRConstNames.put(OpenEHRConst.COMPOSITION, OpenEHRLanguageManager.getMessage("Composition"));
        openEHRConstDescriptions.put(OpenEHRConst.COMPOSITION, OpenEHRLanguageManager.getMessage("CompositionDesc"));
        openEHRConstIcons.put(OpenEHRConst.COMPOSITION, OpenEHRImageUtil.COMPOSITION_ICON);
        openEHRConstIconNames.put(OpenEHRConst.COMPOSITION, OpenEHRImageUtil.COMPOSITION_NAME);

        //Entries

        openEHRConstNames.put(OpenEHRConst.OBSERVATION, OpenEHRLanguageManager.getMessage("EntryObservation"));
        openEHRConstDescriptions.put(OpenEHRConst.OBSERVATION, OpenEHRLanguageManager.getMessage("EntryObservationDesc"));
        openEHRConstIcons.put(OpenEHRConst.OBSERVATION, OpenEHRImageUtil.ENTRY_OBSERVATION_ICON);
        openEHRConstIconNames.put(OpenEHRConst.OBSERVATION, OpenEHRImageUtil.ENTRY_OBSERVATION_NAME);

        openEHRConstNames.put(OpenEHRConst.EVALUATION, OpenEHRLanguageManager.getMessage("EntryEvaluation"));
        openEHRConstDescriptions.put(OpenEHRConst.EVALUATION, OpenEHRLanguageManager.getMessage("EntryEvaluationDesc"));
        openEHRConstIcons.put(OpenEHRConst.EVALUATION, OpenEHRImageUtil.ENTRY_EVALUATION_ICON);
        openEHRConstIconNames.put(OpenEHRConst.EVALUATION, OpenEHRImageUtil.ENTRY_EVALUATION_NAME);

        openEHRConstNames.put(OpenEHRConst.INSTRUCTION, OpenEHRLanguageManager.getMessage("EntryInstruction"));
        openEHRConstDescriptions.put(OpenEHRConst.INSTRUCTION, OpenEHRLanguageManager.getMessage("EntryInstructionDesc"));
        openEHRConstIcons.put(OpenEHRConst.INSTRUCTION, OpenEHRImageUtil.ENTRY_INSTRUCTION_ICON);
        openEHRConstIconNames.put(OpenEHRConst.INSTRUCTION, OpenEHRImageUtil.ENTRY_INSTRUCTION_NAME);

        openEHRConstNames.put(OpenEHRConst.ACTION, OpenEHRLanguageManager.getMessage("EntryAction"));
        openEHRConstDescriptions.put(OpenEHRConst.ACTION, OpenEHRLanguageManager.getMessage("EntryActionDesc"));
        openEHRConstIcons.put(OpenEHRConst.ACTION, OpenEHRImageUtil.ENTRY_ACTION_ICON);
        openEHRConstIconNames.put(OpenEHRConst.ACTION, OpenEHRImageUtil.ENTRY_ACTION_NAME);

        openEHRConstNames.put(OpenEHRConst.ENTRY, OpenEHRLanguageManager.getMessage("Entry"));
        openEHRConstDescriptions.put(OpenEHRConst.ENTRY, OpenEHRLanguageManager.getMessage("Entry"));
        openEHRConstIcons.put(OpenEHRConst.ENTRY, OpenEHRImageUtil.ARCHETYPE);
        openEHRConstIconNames.put(OpenEHRConst.ENTRY, OpenEHRImageUtil.ARCHETYPE_NAME);
    }

    public static String getName(String idDataValue) {
        return getDelegate().openEHRConstNames.get(idDataValue);
    }

    public static String getDescription(String idDataValue) {
        return getDelegate().openEHRConstNames.get(idDataValue);
    }

    public static ImageIcon getIcon(String idDataValue) {
        return getDelegate().openEHRConstIcons.get(idDataValue);
    }

    public static String getIconName(String idDataValue) {
        return getDelegate().openEHRConstIconNames.get(idDataValue);
    }

    public static OpenEHRConstUI getDelegate() {
        if (_instance == null) {
            _instance = new OpenEHRConstUI();
        }
        return _instance;
    }
}
