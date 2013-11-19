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

    public static DvCodedText NULL_FLAVOUR_CODE_NO_INFO = new DvCodedText("no information", new CodePhrase(TerminologyService.OPENEHR, "271"));
    public static DvCodedText NULL_FLAVOUR_CODE_UNKNOWN = new DvCodedText("Unknown", new CodePhrase(TerminologyService.OPENEHR, "253"));
    public static DvCodedText NULL_FLAVOUR_CODE_MASKED = new DvCodedText("Masked", new CodePhrase(TerminologyService.OPENEHR, "272"));
    public static DvCodedText NULL_FLAVOUR_CODE_NOT_APPLICABLE = new DvCodedText("Not applicable", new CodePhrase(TerminologyService.OPENEHR, "273"));


    private final HashMap<String,String> _openEHRConstNames;
    private final HashMap<String,String> _openEHRConstDescriptions;
    private final HashMap<String,ImageIcon> _openEHRConstIcons;
    private final HashMap<String,String> _openEHRConstIconNames;

    public static Map<String, DvCodedText> NULL_FLAVOUR_MAP = new LinkedHashMap<String, DvCodedText>();
    static {
        NULL_FLAVOUR_MAP.put("271", NULL_FLAVOUR_CODE_NO_INFO);
        NULL_FLAVOUR_MAP.put("253", NULL_FLAVOUR_CODE_UNKNOWN);
        NULL_FLAVOUR_MAP.put("272", NULL_FLAVOUR_CODE_MASKED);
        NULL_FLAVOUR_MAP.put("273", NULL_FLAVOUR_CODE_NOT_APPLICABLE);
    }

    private OpenEHRConstUI(){
        _openEHRConstNames = new HashMap<String, String>();
        _openEHRConstDescriptions = new HashMap<String, String>();
        _openEHRConstIcons = new HashMap<String, ImageIcon>();
        _openEHRConstIconNames = new HashMap<String, String>();

        _openEHRConstNames.put(OpenEHRConst.HISTORY, OpenEHRLanguageManager.getMessage("History"));
        _openEHRConstDescriptions.put(OpenEHRConst.HISTORY, OpenEHRLanguageManager.getMessage("HistoryDesc"));
        _openEHRConstIcons.put(OpenEHRConst.HISTORY, OpenEHRImageUtil.STRUCTURE);    //TODO
        _openEHRConstIconNames.put(OpenEHRConst.HISTORY, OpenEHRImageUtil.STRUCTURE_NAME);  //TODO

        _openEHRConstNames.put(OpenEHRConst.EVENT, OpenEHRLanguageManager.getMessage("Event"));
        _openEHRConstDescriptions.put(OpenEHRConst.EVENT, OpenEHRLanguageManager.getMessage("EventDesc"));
        _openEHRConstIcons.put(OpenEHRConst.EVENT, OpenEHRImageUtil.EVENT);
        _openEHRConstIconNames.put(OpenEHRConst.EVENT, OpenEHRImageUtil.EVENT_NAME);

        _openEHRConstNames.put(OpenEHRConst.CLUSTER, OpenEHRLanguageManager.getMessage("Cluster"));
        _openEHRConstDescriptions.put(OpenEHRConst.CLUSTER, OpenEHRLanguageManager.getMessage("ClusterDesc"));
        _openEHRConstIcons.put(OpenEHRConst.CLUSTER, OpenEHRImageUtil.CLUSTER);
        _openEHRConstIconNames.put(OpenEHRConst.CLUSTER, OpenEHRImageUtil.CLUSTER_NAME);

        _openEHRConstNames.put(OpenEHRConst.SECTION, OpenEHRLanguageManager.getMessage("Section"));
        _openEHRConstDescriptions.put(OpenEHRConst.SECTION, OpenEHRLanguageManager.getMessage("SectionDesc"));
        _openEHRConstIcons.put(OpenEHRConst.SECTION, OpenEHRImageUtil.SECTION);
        _openEHRConstIconNames.put(OpenEHRConst.SECTION, OpenEHRImageUtil.SECTION_NAME);

        _openEHRConstNames.put(OpenEHRConst.ACTIVITY, OpenEHRLanguageManager.getMessage("Activity"));
        _openEHRConstDescriptions.put(OpenEHRConst.ACTIVITY, OpenEHRLanguageManager.getMessage("ActivityDesc"));
        _openEHRConstIcons.put(OpenEHRConst.ACTIVITY, OpenEHRImageUtil.ACTIVITY);
        _openEHRConstIconNames.put(OpenEHRConst.ACTIVITY, OpenEHRImageUtil.ACTIVITY_NAME);

        _openEHRConstNames.put(OpenEHRConst.STRUCTURE, OpenEHRLanguageManager.getMessage("Structure"));
        _openEHRConstDescriptions.put(OpenEHRConst.STRUCTURE, OpenEHRLanguageManager.getMessage("StructureDesc"));
        _openEHRConstIcons.put(OpenEHRConst.STRUCTURE, OpenEHRImageUtil.STRUCTURE);
        _openEHRConstIconNames.put(OpenEHRConst.STRUCTURE, OpenEHRImageUtil.STRUCTURE_NAME);

        _openEHRConstNames.put(OpenEHRConst.ITEM_TREE, OpenEHRLanguageManager.getMessage("ItemTree"));
        _openEHRConstDescriptions.put(OpenEHRConst.ITEM_TREE, OpenEHRLanguageManager.getMessage("ItemTreeDesc"));
        _openEHRConstIcons.put(OpenEHRConst.ITEM_TREE, OpenEHRImageUtil.ITEM_TREE);
        _openEHRConstIconNames.put(OpenEHRConst.ITEM_TREE, OpenEHRImageUtil.ITEM_TREE_NAME);

        _openEHRConstNames.put(OpenEHRConst.ITEM_LIST, OpenEHRLanguageManager.getMessage("ItemList"));
        _openEHRConstDescriptions.put(OpenEHRConst.ITEM_LIST, OpenEHRLanguageManager.getMessage("ItemListDesc"));
        _openEHRConstIcons.put(OpenEHRConst.ITEM_LIST, OpenEHRImageUtil.ITEM_LIST);
        _openEHRConstIconNames.put(OpenEHRConst.ITEM_LIST, OpenEHRImageUtil.ITEM_LIST_NAME);

        _openEHRConstNames.put(OpenEHRConst.ITEM_TABLE, OpenEHRLanguageManager.getMessage("ItemTable"));
        _openEHRConstDescriptions.put(OpenEHRConst.ITEM_TABLE, OpenEHRLanguageManager.getMessage("ItemTableDesc"));
        _openEHRConstIcons.put(OpenEHRConst.ITEM_TABLE, OpenEHRImageUtil.ITEM_TABLE);
        _openEHRConstIconNames.put(OpenEHRConst.ITEM_TABLE, OpenEHRImageUtil.ITEM_TABLE_NAME);

        _openEHRConstNames.put(OpenEHRConst.ITEM_SINGLE, OpenEHRLanguageManager.getMessage("ItemSingle"));
        _openEHRConstDescriptions.put(OpenEHRConst.ITEM_SINGLE, OpenEHRLanguageManager.getMessage("ItemSingleDesc"));
        _openEHRConstIcons.put(OpenEHRConst.ITEM_SINGLE, OpenEHRImageUtil.ITEM_SINGLE);
        _openEHRConstIconNames.put(OpenEHRConst.ITEM_SINGLE, OpenEHRImageUtil.ITEM_SINGLE_NAME);


        _openEHRConstNames.put(OpenEHRConst.ELEMENT, OpenEHRLanguageManager.getMessage("Element"));
        _openEHRConstDescriptions.put(OpenEHRConst.ELEMENT, OpenEHRLanguageManager.getMessage("ElementDesc"));
        _openEHRConstIcons.put(OpenEHRConst.ELEMENT, OpenEHRImageUtil.ELEMENT);
        _openEHRConstIconNames.put(OpenEHRConst.ELEMENT, OpenEHRImageUtil.ELEMENT_NAME);

        _openEHRConstNames.put(OpenEHRConst.COMPOSITION, OpenEHRLanguageManager.getMessage("Composition"));
        _openEHRConstDescriptions.put(OpenEHRConst.COMPOSITION, OpenEHRLanguageManager.getMessage("CompositionDesc"));
        _openEHRConstIcons.put(OpenEHRConst.COMPOSITION, OpenEHRImageUtil.COMPOSITION_ICON);
        _openEHRConstIconNames.put(OpenEHRConst.COMPOSITION, OpenEHRImageUtil.COMPOSITION_NAME);

        //Entries

        _openEHRConstNames.put(OpenEHRConst.OBSERVATION, OpenEHRLanguageManager.getMessage("EntryObservation"));
        _openEHRConstDescriptions.put(OpenEHRConst.OBSERVATION, OpenEHRLanguageManager.getMessage("EntryObservationDesc"));
        _openEHRConstIcons.put(OpenEHRConst.OBSERVATION, OpenEHRImageUtil.ENTRY_OBSERVATION_ICON);
        _openEHRConstIconNames.put(OpenEHRConst.OBSERVATION, OpenEHRImageUtil.ENTRY_OBSERVATION_NAME);

        _openEHRConstNames.put(OpenEHRConst.EVALUATION, OpenEHRLanguageManager.getMessage("EntryEvaluation"));
        _openEHRConstDescriptions.put(OpenEHRConst.EVALUATION, OpenEHRLanguageManager.getMessage("EntryEvaluationDesc"));
        _openEHRConstIcons.put(OpenEHRConst.EVALUATION, OpenEHRImageUtil.ENTRY_EVALUATION_ICON);
        _openEHRConstIconNames.put(OpenEHRConst.EVALUATION, OpenEHRImageUtil.ENTRY_EVALUATION_NAME);

        _openEHRConstNames.put(OpenEHRConst.INSTRUCTION, OpenEHRLanguageManager.getMessage("EntryInstruction"));
        _openEHRConstDescriptions.put(OpenEHRConst.INSTRUCTION, OpenEHRLanguageManager.getMessage("EntryInstructionDesc"));
        _openEHRConstIcons.put(OpenEHRConst.INSTRUCTION, OpenEHRImageUtil.ENTRY_INSTRUCTION_ICON);
        _openEHRConstIconNames.put(OpenEHRConst.INSTRUCTION, OpenEHRImageUtil.ENTRY_INSTRUCTION_NAME);

        _openEHRConstNames.put(OpenEHRConst.ACTION, OpenEHRLanguageManager.getMessage("EntryAction"));
        _openEHRConstDescriptions.put(OpenEHRConst.ACTION, OpenEHRLanguageManager.getMessage("EntryActionDesc"));
        _openEHRConstIcons.put(OpenEHRConst.ACTION, OpenEHRImageUtil.ENTRY_ACTION_ICON);
        _openEHRConstIconNames.put(OpenEHRConst.ACTION, OpenEHRImageUtil.ENTRY_ACTION_NAME);
    }

    public static String getName(String idDataValue){
        return getDelegate()._openEHRConstNames.get(idDataValue);
    }

    public static String getDescription(String idDataValue){
        return getDelegate()._openEHRConstNames.get(idDataValue);
    }

    public static ImageIcon getIcon(String idDataValue){
        return getDelegate()._openEHRConstIcons.get(idDataValue);
    }

    public static String getIconName(String idDataValue){
        return getDelegate()._openEHRConstIconNames.get(idDataValue);
    }

    public static OpenEHRConstUI getDelegate(){
        if (_instance == null){
            _instance = new OpenEHRConstUI();
        }
        return _instance;
    }
}
