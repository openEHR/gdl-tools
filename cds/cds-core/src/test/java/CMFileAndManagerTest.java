import org.junit.Before;
import org.junit.Test;
import se.cambio.cds.controller.session.data.Studies;
import se.cambio.cm.model.study.dto.StudyDTO;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.net.URISyntaxException;
import java.util.Collection;

import static org.junit.Assert.assertEquals;

public class CMFileAndManagerTest {

    @Before
    public void loadCM() throws URISyntaxException, InternalErrorException {
        CMUtil.testLoadCM();
    }

    @Test
    public void shouldRetrieveStudiesAndFindChanges() throws InternalErrorException {
        Studies.getInstance().loadAll();
        Collection<StudyDTO> studies = Studies.getInstance().getAllInCache();
        assertEquals(1, studies.size());
    }
}
