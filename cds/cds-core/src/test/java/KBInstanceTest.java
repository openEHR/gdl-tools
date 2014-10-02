import org.junit.Test;
import org.openehr.am.archetype.Archetype;
import org.openehr.rm.common.archetyped.Locatable;
import org.openehr.rm.util.GenerationStrategy;
import org.openehr.rm.util.SkeletonGenerator;
import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.cds.util.export.json.JSONSerialization;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.util.IOUtils;

import java.io.InputStream;
import java.nio.charset.Charset;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class KBInstanceTest {

    @Test
    public void shouldHaveEqualValueAfterRoundTrip() throws Exception {
        CMUtil.testLoadCM();
        KBInstance kbInstance = new KBInstance("testKBInstance");
        String templateId = "diagnosis_icd10";
        Archetype archetype = Templates.getTemplateAOM(templateId);
        Object obj = SkeletonGenerator.getInstance().create(archetype, templateId, Archetypes.getArchetypeMap(), GenerationStrategy.MINIMUM);
        assertTrue(obj instanceof Locatable);
        kbInstance.setLocatable((Locatable)obj);
        String kbiSerialized = JSONSerialization.serialize(KBInstance.class, kbInstance);
        InputStream is = KBInstanceTest.class.getClassLoader().getResourceAsStream("kbInstance/diagnosisTest.json");
        byte[] kbiInFileBytes = IOUtils.toByteArray(is);
        String kbiInFileStr = new String(kbiInFileBytes, Charset.forName("UTF8"));
        assertEquals(kbiInFileStr.trim().replaceAll("\\r\\n|\\r|\\n", ""), kbiSerialized.trim().replaceAll("\\r\\n|\\r|\\n", ""));
    }
}
