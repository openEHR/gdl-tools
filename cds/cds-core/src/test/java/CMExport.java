import org.junit.Test;
import se.cambio.cds.util.CMImportExportManager;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class CMExport {
    @Test
    public void shouldExportCM() throws InternalErrorException, InstanceNotFoundException, IOException {
        File cmFile = new File("test.cm");
        new CMImportExportManager().exportCurrentCM(new FileOutputStream(cmFile));
        Files.delete(Paths.get(cmFile.toURI()));
    }
}
