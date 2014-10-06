package se.cambio.openehr.controller;

import org.apache.log4j.Logger;
import org.openehr.am.archetype.Archetype;
import se.acode.openehr.parser.ADLParser;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.vo.ArchetypeObjectBundleCustomVO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InternalErrorException;

public class ArchetypeObjectBundleManager {
    private ArchetypeDTO archetypeDTO = null;
    protected boolean correctlyParsed = false;

    public ArchetypeObjectBundleManager(ArchetypeDTO archetypeDTO) {
        this.archetypeDTO = archetypeDTO;
        Object obj = null;
        if (archetypeDTO.getAobcVO() != null){
            obj = IOUtils.getObject(archetypeDTO.getAobcVO());
        }
        if (!(obj instanceof ArchetypeObjectBundleCustomVO)){
            Logger.getLogger(ArchetypeObjectBundleManager.class).debug("Parsing archetype '"+archetypeDTO.getArchetypeId()+"'...");
            try{
                generateArchetypeObjectBundleCustomVO();
                correctlyParsed = true;
            }catch(Error e){
                InternalErrorException iee = new InternalErrorException(new Exception("Failed to parse archetype '"+archetypeDTO.getArchetypeId()+"'", e));
                ExceptionHandler.handle(iee);
            }catch(Exception e){
                InternalErrorException iee = new InternalErrorException(e);
                ExceptionHandler.handle(iee);
            }
        }else{
            correctlyParsed = true;
        }
    }


    public void generateArchetypeObjectBundleCustomVO ()
            throws InternalErrorException{
        ADLParser adlParser = new ADLParser(archetypeDTO.getArchetype());
        Archetype ar = null;
        try {
            ar = adlParser.parse();
        } catch (Exception e) {
            throw new InternalErrorException(e);
        }
        archetypeDTO.setRMName(ar.getArchetypeId().rmEntity());
        archetypeDTO.setAom(IOUtils.getBytes(ar));        GenericObjectBundleManager genericObjectBundleManager = new GenericObjectBundleManager(ar);
        ArchetypeObjectBundleCustomVO archetypeObjectBundleCustomVO = genericObjectBundleManager.generateObjectBundleCustomVO();
        archetypeDTO.setAobcVO(IOUtils.getBytes(archetypeObjectBundleCustomVO));
    }
}
