package se.cambio.openehr.util;

import org.openehr.rm.datatypes.quantity.ProportionKind;

import java.util.HashMap;
import java.util.Map;

public class ProportionTypesConst {

    private static ProportionTypesConst instance;
    private Map<ProportionKind, String> proportionTypeNames = null;
    private Map<ProportionKind, String> proportionTypeDescriptions = null;
    private Map<ProportionKind, String> proportionTypeIDs = null;

    private static final String RATIO_ID = "RATIO";
    private static final String UNITARY_ID = "UNITARY";
    private static final String PERCENT_ID = "PERCENT";
    private static final String FRACTION_ID = "FRACTION";
    private static final String INTEGER_FRACTION_ID = "INTEGER_FRACTION";

    private ProportionTypesConst() {
        proportionTypeIDs = new HashMap<>();

        proportionTypeNames = new HashMap<>();
        proportionTypeDescriptions = new HashMap<>();
        proportionTypeNames.put(ProportionKind.RATIO, OpenEHRLanguageManager.getMessage("Ratio"));
        proportionTypeNames.put(ProportionKind.UNITARY, OpenEHRLanguageManager.getMessage("Unitary"));
        proportionTypeNames.put(ProportionKind.PERCENT, OpenEHRLanguageManager.getMessage("Percent"));
        proportionTypeNames.put(ProportionKind.FRACTION, OpenEHRLanguageManager.getMessage("Fraction"));
        proportionTypeNames.put(ProportionKind.INTEGER_FRACTION, OpenEHRLanguageManager.getMessage("IntegerFraction"));

        proportionTypeDescriptions.put(ProportionKind.RATIO, OpenEHRLanguageManager.getMessage("RatioDesc"));
        proportionTypeDescriptions.put(ProportionKind.UNITARY, OpenEHRLanguageManager.getMessage("UnitaryDesc"));
        proportionTypeDescriptions.put(ProportionKind.PERCENT, OpenEHRLanguageManager.getMessage("PercentDesc"));
        proportionTypeDescriptions.put(ProportionKind.FRACTION, OpenEHRLanguageManager.getMessage("FractionDesc"));
        proportionTypeDescriptions.put(ProportionKind.INTEGER_FRACTION, OpenEHRLanguageManager.getMessage("IntegerFractionDesc"));

        proportionTypeIDs.put(ProportionKind.RATIO, ProportionKind.class.getSimpleName() + "." + RATIO_ID);
        proportionTypeIDs.put(ProportionKind.UNITARY, ProportionKind.class.getSimpleName() + "." + UNITARY_ID);
        proportionTypeIDs.put(ProportionKind.PERCENT, ProportionKind.class.getSimpleName() + "." + PERCENT_ID);
        proportionTypeIDs.put(ProportionKind.FRACTION, ProportionKind.class.getSimpleName() + "." + FRACTION_ID);
        proportionTypeIDs.put(ProportionKind.INTEGER_FRACTION, ProportionKind.class.getSimpleName() + "." + INTEGER_FRACTION_ID);
    }


    public String getName(ProportionKind proportionType) {
        return proportionTypeNames.get(proportionType);
    }

    public String getDescription(ProportionKind proportionType) {
        return proportionTypeDescriptions.get(proportionType);
    }

    public String getInstanceID(ProportionKind proportionKind) {
        return proportionTypeIDs.get(proportionKind);
    }

    public static ProportionTypesConst getInstance() {
        if (instance == null) {
            instance = new ProportionTypesConst();
        }
        return instance;
    }
}
