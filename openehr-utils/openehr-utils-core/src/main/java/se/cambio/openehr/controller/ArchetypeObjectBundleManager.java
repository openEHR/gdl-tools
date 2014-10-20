package se.cambio.openehr.controller;

import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import se.acode.openehr.parser.ADLParser;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Map;

public class ArchetypeObjectBundleManager {
    private ArchetypeDTO archetypeDTO = null;
    protected boolean correctlyParsed = false;
    private Map<String, Archetype> archetypeMap;

    public ArchetypeObjectBundleManager(ArchetypeDTO archetypeDTO, Map<String, Archetype> archetypeMap) {
        this.archetypeDTO = archetypeDTO;
        this.archetypeMap = archetypeMap;
    }

    public void buildArchetypeObjectBundleCustomVO() {
        Object obj = null;
        if (archetypeDTO.getAobcVO() != null){
            obj = IOUtils.getObject(archetypeDTO.getAobcVO());
        }
        if (!(obj instanceof ArchetypeObjectBundleCustomVO)){
            Logger.getLogger(ArchetypeObjectBundleManager.class).debug("Parsing archetype '"+archetypeDTO.getId()+"'...");
            try{
                generateArchetypeData();
                correctlyParsed = true;
            }catch(Error e){
                InternalErrorException iee = new InternalErrorException(new Exception("Failed to parse archetype '"+archetypeDTO.getId()+"'", e));
                ExceptionHandler.handle(iee);
            }catch(Exception e){
                InternalErrorException iee = new InternalErrorException(e);
                ExceptionHandler.handle(iee);
            }
        }else{
            correctlyParsed = true;
        }
    }


    private void generateArchetypeData()
            throws InternalErrorException{
        try{
            ADLParser adlParser = new ADLParser(archetypeDTO.getSource());
            Archetype ar = adlParser.parse();
            archetypeDTO.setAom(IOUtils.getBytes(ar));
            GenericObjectBundleManager genericObjectBundleManager = new GenericObjectBundleManager(ar, archetypeMap);
            ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = genericObjectBundleManager.generateObjectBundleCustomVO();
            archetypeDTO.setAobcVO(IOUtils.getBytes(archetypeObjectBundleCustomVO));
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
    }
}
