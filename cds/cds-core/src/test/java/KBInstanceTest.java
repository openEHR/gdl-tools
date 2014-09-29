import junit.framework.TestCase;
import org.openehr.am.archetype.Archetype;
import org.openehr.rm.common.archetyped.Locatable;
import org.openehr.rm.util.GenerationStrategy;
import org.openehr.rm.util.SkeletonGenerator;
import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.cds.util.export.json.JSONSerialization;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;

import java.io.InputStream;
import java.nio.charset.Charset;

/**
 * User: Iago.Corbal
 * Date: 2014-09-29
 * Time: 09:05
 */
public class KBInstanceTest extends TestCase{

    public void testKBInstanceRoundTrip(){
        CMUtil.testLoadCM();
        KBInstance kbInstance = new KBInstance();
        kbInstance.setKbiId("testKBInstance");
        String templateId = "diagnosis_icd10";
        Archetype archetype = Templates.getTemplateAOM(templateId);
        try {
            Object obj = SkeletonGenerator.getInstance().create(archetype, templateId, Archetypes.getArchetypeMap(), GenerationStrategy.MINIMUM);
            assertTrue(obj instanceof Locatable);
            kbInstance.setLocatable((Locatable)obj);
            String kbiSerialized = JSONSerialization.serialize(KBInstance.class, kbInstance);
            InputStream is = KBInstanceTest.class.getClassLoader().getResourceAsStream("kbInstance/diagnosisTest.json");
            byte[] kbiInFileBytes = IOUtils.toByteArray(is);
            String kbiInFileStr = new String(kbiInFileBytes, Charset.forName("UTF8"));
            assertEquals(kbiInFileStr.trim().replaceAll("\\r\\n|\\r|\\n", ""), kbiSerialized.trim().replaceAll("\\r\\n|\\r|\\n", ""));
        } catch (Exception e) {
            ExceptionHandler.handle(e);
            fail();
        }
    }
}
