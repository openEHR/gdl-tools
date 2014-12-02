import se.cambio.cds.gdl.model.Guide;
import se.cambio.cds.util.GuideCompiler;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class DummyGuideCompiler implements GuideCompiler {
    @Override
    public byte[] compile(Guide guide) throws InternalErrorException {
        return new byte[0];
    }
}
