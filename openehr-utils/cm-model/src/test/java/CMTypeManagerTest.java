import org.junit.Test;
import se.cambio.cm.model.util.CMType;
import se.cambio.cm.model.util.CMTypeManager;

import java.util.Iterator;

import static org.junit.Assert.assertEquals;

public class CMTypeManagerTest {
    @Test
    public void shouldHaveOrderOnCMTypes(){
        Iterator<CMType> cmTypeIterator = CMTypeManager.getInstance().getAllCMTypes().iterator();
        assertEquals("terminologies", cmTypeIterator.next().getId());
        assertEquals("archetypes", cmTypeIterator.next().getId());
        assertEquals("templates", cmTypeIterator.next().getId());
        assertEquals("guidelines", cmTypeIterator.next().getId());
    }
}
