package se.cambio.cds.util.comparators;

import se.cambio.openehr.model.util.CMElement;

import java.util.Comparator;

public class CMElementComparator implements Comparator<CMElement> {
    @Override
    public int compare(CMElement o1, CMElement o2) {
        return o1.getId().compareTo(o2.getId());
    }
}
