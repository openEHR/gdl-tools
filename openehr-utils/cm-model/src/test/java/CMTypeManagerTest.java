import org.junit.Test;
import se.cambio.cm.model.archetype.dto.ArchetypeDTO;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.cm.model.template.dto.TemplateDTO;
import se.cambio.cm.model.terminology.dto.TerminologyDTO;
import se.cambio.cm.model.util.CMType;
import se.cambio.cm.model.util.CMTypeManager;

import java.lang.reflect.ParameterizedType;
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

    @Test
    public void shouldFindArchetypeCMType() {
        CMType cmTypeByClass = CMTypeManager.getInstance().getCMTypeByClass(ArchetypeDTO.class);
        assertEquals("archetypes", cmTypeByClass.getId());
    }

    @Test
    public void shouldFindTemplateCMType() {
        CMType cmTypeByClass = CMTypeManager.getInstance().getCMTypeByClass(TemplateDTO.class);
        assertEquals("templates", cmTypeByClass.getId());
    }

    @Test
    public void shouldFindTerminologyCMType() {
        CMType cmTypeByClass = CMTypeManager.getInstance().getCMTypeByClass(TerminologyDTO.class);
        assertEquals("terminologies", cmTypeByClass.getId());
    }

    @Test
    public void shouldFindGuidelinesCMTypeByClass() {
        CMType cmTypeByClass = CMTypeManager.getInstance().getCMTypeByClass(GuideDTO.class);
        assertEquals("guidelines", cmTypeByClass.getId());
    }

    @Test
    public void shouldFindGuidelinesCMTypeById() {
        CMType cmTypeByClass = CMTypeManager.getInstance().getCMTypeById("guidelines");
        assertEquals("guidelines", cmTypeByClass.getId());
    }

    @Test
    public void shouldFindArchetypesCMTypeById() {
        CMType cmTypeByClass = CMTypeManager.getInstance().getCMTypeById("archetypes");
        assertEquals("archetypes", cmTypeByClass.getId());
    }

    @Test
    public void shouldFindGuidelinesCMElement() {
        Class cmElementClass = CMTypeManager.getInstance().getCMElementClassById("guidelines");
        assertEquals("se.cambio.cm.model.guide.dto.GuideDTO", cmElementClass.getName());
    }

    @Test
    public void shouldFindArchetypesCMElement() {
        Class cmElementClass = CMTypeManager.getInstance().getCMElementClassById("archetypes");
        assertEquals("se.cambio.cm.model.archetype.dto.ArchetypeDTO", cmElementClass.getName());
    }

    @Test
    public void shouldGetCmElementListParameterizedType() {
        ParameterizedType cmElementListParameterizedType = CMTypeManager.getInstance().getCmElementListParameterizedType(GuideDTO.class);
        assertEquals(1, cmElementListParameterizedType.getActualTypeArguments().length);
        assertEquals("se.cambio.cm.model.guide.dto.GuideDTO", cmElementListParameterizedType.getActualTypeArguments()[0].getTypeName());

    }
}
