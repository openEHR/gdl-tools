import junit.framework.TestCase;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.HTMLRenderer;

import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;

/**
 * User: Iago.Corbal
 * Date: 2014-09-10
 * Time: 17:17
 */
public class FreeMarkerTest extends TestCase{

    public void testRenderer(){
        String src = "<html>\n" +
                "<head>\n" +
                "  <title>Welcome!</title>\n" +
                "</head>\n" +
                "<body>\n" +
                "  <h1>Welcome ${user}!</h1>\n" +
                "  <p>test name: ${testRoot[\"key\"?eval]}\n</p>" +
                "</body>\n" +
                "</html> ";
        Map<String, Object> root = new HashMap<String, Object>();
        root.put("user", "Big Joe");
        root.put("key", "testKey");

        Map latest = new HashMap();
        root.put("testRoot", latest);
        latest.put("testKey", "test1");
        try {
            HTMLRenderer cmHTMLr = new HTMLRenderer(new StringReader(src));
            String result = cmHTMLr.proccess(root);
            assertNotNull(result);
            assertTrue(result.contains("Welcome Big Joe!"));
            assertTrue(result.contains("test1"));
        } catch (InternalErrorException e) {
            e.printStackTrace();
        }
    }
}
