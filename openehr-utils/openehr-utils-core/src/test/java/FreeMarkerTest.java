import org.testng.annotations.Test;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.HTMLRenderer;

import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.IsNull.notNullValue;


public class FreeMarkerTest {

    @Test
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
            String result = cmHTMLr.process(root);
            assertThat(result, notNullValue());
            assertThat(result, containsString("Welcome Big Joe!"));
            assertThat(result, containsString("test1"));
        } catch (InternalErrorException ex) {
            ex.printStackTrace();
        }
    }
}
