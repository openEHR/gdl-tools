package se.cambio.cds.util.comparators;

import se.cambio.cds.model.instance.ElementInstance;

import java.util.Comparator;

/**
 * User: Iago.Corbal
 * Date: 2014-01-18
 * Time: 15:21
 */
public class ElementInstancePathDVComparator implements Comparator<ElementInstance> {
    @Override
    public int compare(ElementInstance o1, ElementInstance o2) {
        int c = o1.getId().compareTo(o2.getId());
        if (c==0){
            if (o2.getDataValue()==null){
                return 1;
            }else if (o1.getDataValue()==null){
                return -1;
            }else{
                String sdvo1 = o1.getDataValue().serialise();
                String sdvo2 = o2.getDataValue().serialise();
                if (sdvo2==null){
                    return 1;
                }else if (sdvo1==null){
                    return -1;
                }else{
                    return sdvo1.compareTo(sdvo2);
                }
            }
        }else{
            return c;
        }
    }
}
