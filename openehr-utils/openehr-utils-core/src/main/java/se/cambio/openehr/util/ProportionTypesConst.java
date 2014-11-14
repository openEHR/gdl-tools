package se.cambio.openehr.util;

import org.openehr.rm.datatypes.quantity.ProportionKind;

import java.util.HashMap;
import java.util.Map;

public class ProportionTypesConst {

    private static ProportionTypesConst instance;
    private Map<ProportionKind, String> _proportionTypeNames = null;
    private Map<ProportionKind, String> _proportionTypeDescriptions = null;
    private Map<ProportionKind, String> _proportionTypeIDs = null;

    private String RATIO_ID = "RATIO";
    private String UNITARY_ID = "UNITARY";
    private String PERCENT_ID = "PERCENT";
    private String FRACTION_ID = "FRACTION";
    private String INTEGER_FRACTION_ID = "INTEGER_FRACTION";

    private ProportionTypesConst(){
        _proportionTypeIDs = new HashMap<ProportionKind, String>();

        _proportionTypeNames = new HashMap<ProportionKind, String>();
        _proportionTypeDescriptions = new HashMap<ProportionKind, String>();
        _proportionTypeNames.put(ProportionKind.RATIO, OpenEHRLanguageManager.getMessage("Ratio"));
        _proportionTypeNames.put(ProportionKind.UNITARY, OpenEHRLanguageManager.getMessage("Unitary"));
        _proportionTypeNames.put(ProportionKind.PERCENT, OpenEHRLanguageManager.getMessage("Percent"));
        _proportionTypeNames.put(ProportionKind.FRACTION, OpenEHRLanguageManager.getMessage("Fraction"));
        _proportionTypeNames.put(ProportionKind.INTEGER_FRACTION, OpenEHRLanguageManager.getMessage("IntegerFraction"));

        _proportionTypeDescriptions.put(ProportionKind.RATIO, OpenEHRLanguageManager.getMessage("RatioDesc"));
        _proportionTypeDescriptions.put(ProportionKind.UNITARY, OpenEHRLanguageManager.getMessage("UnitaryDesc"));
        _proportionTypeDescriptions.put(ProportionKind.PERCENT, OpenEHRLanguageManager.getMessage("PercentDesc"));
        _proportionTypeDescriptions.put(ProportionKind.FRACTION, OpenEHRLanguageManager.getMessage("FractionDesc"));
        _proportionTypeDescriptions.put(ProportionKind.INTEGER_FRACTION, OpenEHRLanguageManager.getMessage("IntegerFractionDesc"));

        _proportionTypeIDs.put(ProportionKind.RATIO, ProportionKind.class.getSimpleName()+"."+RATIO_ID);
        _proportionTypeIDs.put(ProportionKind.UNITARY, ProportionKind.class.getSimpleName()+"."+UNITARY_ID);
        _proportionTypeIDs.put(ProportionKind.PERCENT, ProportionKind.class.getSimpleName()+"."+PERCENT_ID);
        _proportionTypeIDs.put(ProportionKind.FRACTION, ProportionKind.class.getSimpleName()+"."+FRACTION_ID);
        _proportionTypeIDs.put(ProportionKind.INTEGER_FRACTION, ProportionKind.class.getSimpleName()+"."+INTEGER_FRACTION_ID);
    }


    public String getName(ProportionKind proportionType){
        return _proportionTypeNames.get(proportionType);
    }

    public String getDescription(ProportionKind proportionType){
        return _proportionTypeDescriptions.get(proportionType);
    }

    public String getInstanceID(ProportionKind proportionKind){
        return _proportionTypeIDs.get(proportionKind);
    }

    public static ProportionTypesConst getInstance() {
        if (instance == null) {
            instance = new ProportionTypesConst();
        }
        return instance;
    }
}
