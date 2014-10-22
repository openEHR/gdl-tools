package se.cambio.cds.util.export.json;

import com.google.gson.ExclusionStrategy;
import com.google.gson.FieldAttributes;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.apache.commons.lang.StringUtils;
import org.openehr.am.parser.ContentObject;
import org.openehr.am.parser.DADLParser;
import org.openehr.am.parser.ParseException;
import org.openehr.build.RMObjectBuildingException;
import org.openehr.rm.binding.DADLBinding;
import org.openehr.rm.binding.DADLBindingException;
import org.openehr.rm.common.archetyped.Locatable;
import se.cambio.cds.model.kb.instance.KBInstance;
import se.cambio.cds.util.export.json.util.KBInstanceWithSource;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.List;

public class KBInstanceSerializer implements JSONSerializer<KBInstance> {
    private Gson gson;

    public KBInstanceSerializer() {
        ExclusionStrategy excludeStrings = new ExclusionStrategy() {
            @Override
            public boolean shouldSkipField(FieldAttributes f) {
                return false;
            }

            @Override
            public boolean shouldSkipClass(Class<?> clazz) {
                if (clazz.isAssignableFrom(Locatable.class)){
                    return true;
                }else {
                    return false;
                }
            }
        };
        gson = new GsonBuilder().setPrettyPrinting()
                .setExclusionStrategies(excludeStrings)
                .create();
    }

    @Override
    public KBInstance parse(String src) throws InternalErrorException {
        KBInstanceWithSource kbInstance = gson.fromJson(src, KBInstanceWithSource.class);
        try {
            if (kbInstance.getLocatableSrc()!=null) {
                ContentObject contentObject = new DADLParser(kbInstance.getLocatableSrc()).parse();
                Locatable locatable = (Locatable) new DADLBinding().bind(contentObject);
                kbInstance.setLocatable(locatable);
            }
        } catch (ParseException e) {
            throw new InternalErrorException(e);
        } catch (RMObjectBuildingException e) {
            throw new InternalErrorException(e);
        } catch (DADLBindingException e) {
            throw new InternalErrorException(e);
        }
        return kbInstance;
    }

    @Override
    public String serialize(KBInstance entity) throws InternalErrorException {
        try {
            String locatableStr = null;
            if (entity.getLocatable()!=null) {
                List<String> dadlLines = new DADLBinding().toDADL(entity.getLocatable());
                locatableStr = StringUtils.join(dadlLines, "\n");
            }
            KBInstanceWithSource kbInstanceWithSource = new KBInstanceWithSource(entity.getKbiId());
            kbInstanceWithSource.setLocatableSrc(locatableStr);
            kbInstanceWithSource.setKbInstanceDefinitions(entity.getKbInstanceDefinitions());
            kbInstanceWithSource.setResourceDescription(entity.getResourceDescription());
            kbInstanceWithSource.setLanguage(entity.getLanguage());
            return gson.toJson(kbInstanceWithSource);
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }
}
