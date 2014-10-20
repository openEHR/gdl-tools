package se.cambio.cds.util;

import se.cambio.cds.controller.session.data.DecisionSupportViews;
import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.view.dto.DSViewDTO;
import se.cambio.openehr.controller.session.data.ArchetypeManager;
import se.cambio.openehr.controller.session.data.Terminologies;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTOBuilder;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.template.dto.TemplateDTOBuilder;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.*;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * User: Iago.Corbal
 * Date: 2013-10-28
 * Time: 17:28
 */
public class CMImportExportManager {

    private static String ARCHETYPES_FOLDER_NAME = "archetypes";
    private static String TEMPLATES_FOLDER_NAME = "templates";
    private static String TERMINOLOGIES_FOLDER_NAME = "terminologies";
    private static String GUIDELINES_FOLDER_NAME = "guidelines";
    private static String DSVIEWS_FOLDER_NAME = "views";

    private static String ARCHETYPE_POSTFIX = ".adl";
    private static String TEMPLATES_POSTFIX = ".oet";
    private static String TERMINOLOGY_POSTFIX = ".csv";
    private static String GUIDELINES_POSTFIX = ".gdl";
    private static String DSVIEWS_POSTFIX = ".dsv";
    private static String DTO_POSTFIX = ".dto";

    private static String ARCHETYPE_PREFIX = ARCHETYPES_FOLDER_NAME + "\\";
    private static String TEMPLATES_PREFIX = TEMPLATES_FOLDER_NAME + "\\";
    private static String TERMINOLOGY_PREFIX = TERMINOLOGIES_FOLDER_NAME + "\\";
    private static String GUIDELINES_PREFIX = GUIDELINES_FOLDER_NAME + "\\";
    private static String DSVVIEWS_PREFIX = DSVIEWS_FOLDER_NAME  + "\\";

    private static String ARCHETYPE_DTO_PREFIX = ARCHETYPES_FOLDER_NAME + DTO_POSTFIX;
    private static String TEMPLATES_DTO_PREFIX = TEMPLATES_FOLDER_NAME + DTO_POSTFIX;
    private static String GUIDELINES_DTO_PREFIX = GUIDELINES_FOLDER_NAME + DTO_POSTFIX;

    public static enum GenerationStrategy {DTOS, NONE}

    public static void exportCurrentCM(File file) throws IOException, InternalErrorException, InstanceNotFoundException {
        exportCurrentCM(file, GenerationStrategy.NONE);
    }

    public static void exportCurrentCM(File file, GenerationStrategy generationStrategy) throws IOException, InstanceNotFoundException, InternalErrorException {
        ZipOutputStream out = new ZipOutputStream(new FileOutputStream(file));
        exportArchetypes(out, generationStrategy);
        exportTemplates(out, generationStrategy);
        exportTerminologies(out);
        exportGuidelines(out, generationStrategy);
        exportViews(out);
        out.close();
    }

    public static void exportArchetypes(ZipOutputStream out, GenerationStrategy generationStrategy) throws IOException {
        for (ArchetypeDTO archetypeDTO: ArchetypeManager.getInstance().getArchetypes().getAllInCache()){
            InputStream in = new ByteArrayInputStream(archetypeDTO.getSource().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(ARCHETYPE_PREFIX + archetypeDTO.getId() + ARCHETYPE_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
            if (GenerationStrategy.DTOS.equals(generationStrategy)){
                out.putNextEntry(new ZipEntry(ARCHETYPE_DTO_PREFIX + "\\" + archetypeDTO.getId() + DTO_POSTFIX));
                in = new ByteArrayInputStream(IOUtils.getBytes(archetypeDTO));
                while ((count = in.read(b)) > 0) {
                    out.write(b, 0, count);
                }
                in.close();
            }
        }
    }

    public static void exportTemplates(ZipOutputStream out, GenerationStrategy generationStrategy) throws IOException {
        for (TemplateDTO templateDTO: ArchetypeManager.getInstance().getTemplates().getAllInCache()){
            InputStream in = new ByteArrayInputStream(templateDTO.getSource().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(TEMPLATES_PREFIX + templateDTO.getId() + TEMPLATES_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
            if (GenerationStrategy.DTOS.equals(generationStrategy)){
                out.putNextEntry(new ZipEntry(TEMPLATES_DTO_PREFIX + "\\" + templateDTO.getId() + DTO_POSTFIX));
                in = new ByteArrayInputStream(IOUtils.getBytes(templateDTO));
                while ((count = in.read(b)) > 0) {
                    out.write(b, 0, count);
                }
                in.close();
            }
        }
    }

    public static void exportTerminologies(ZipOutputStream out) throws IOException {
        for (TerminologyDTO terminologyDTO: Terminologies.getInstance().getAllInCache()){
            InputStream in = new ByteArrayInputStream(terminologyDTO.getSource().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(TERMINOLOGY_PREFIX + terminologyDTO.getId() + TERMINOLOGY_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
        }
    }

    public static void exportGuidelines(ZipOutputStream out, GenerationStrategy generationStrategy) throws IOException, InternalErrorException, InstanceNotFoundException {
        Collection<String> guideIds = Guides.getInstance().getAllIds();
        for (GuideDTO guideDTO: Guides.getInstance().getCMElementByIds(guideIds)){
            InputStream in = new ByteArrayInputStream(guideDTO.getSource().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(GUIDELINES_PREFIX + guideDTO.getId() + GUIDELINES_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
            if (GenerationStrategy.DTOS.equals(generationStrategy)){
                out.putNextEntry(new ZipEntry(GUIDELINES_DTO_PREFIX + "\\" + guideDTO.getId() + DTO_POSTFIX));
                in = new ByteArrayInputStream(IOUtils.getBytes(guideDTO));
                while ((count = in.read(b)) > 0) {
                    out.write(b, 0, count);
                }
                in.close();
            }
        }
    }

    public static void exportViews(ZipOutputStream out) throws IOException {
        for (DSViewDTO DSViewDTO : DecisionSupportViews.getInstance().getAllDSViews()){
            InputStream in = new ByteArrayInputStream(DSViewDTO.getDSViewSrc().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(DSVVIEWS_PREFIX + DSViewDTO.getDsViewId() + DSVIEWS_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
        }
    }


    /* WARNING! Guides imported using this method will not be compiled! */
    public static void importCM(InputStream is) throws IOException {

        Collection<ArchetypeDTO> archetypeSourceDTOs = new ArrayList<ArchetypeDTO>();
        Collection<ArchetypeDTO> archetypeDTOs = new ArrayList<ArchetypeDTO>();
        Collection<TemplateDTO> templateSourceDTOs = new ArrayList<TemplateDTO>();
        Collection<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>();
        Collection<TerminologyDTO> terminologyDTOs = new ArrayList<TerminologyDTO>();
        Collection<DSViewDTO> DSViewDTOs = new ArrayList<DSViewDTO>();
        Collection<GuideDTO> guideSourceDTOs = new ArrayList<GuideDTO>();
        Collection<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();

        boolean useArchetypeDTOs = false;
        boolean useTemplateDTOs = false;
        boolean useGuidelineDTOs = false;

        BufferedInputStream bis = new BufferedInputStream(is);
        //bis.mark(4048);
        //Look for DTO folders
        ZipInputStream zis = new ZipInputStream(bis);
        try{
            ZipEntry entry;
            while((entry = zis.getNextEntry())!=null){
                if (entry.getName().startsWith(ARCHETYPES_FOLDER_NAME) && entry.getName().endsWith(ARCHETYPE_POSTFIX)){
                    String src = IOUtils.toString(zis,"UTF-8");
                    String archetypeId = entry.getName().substring(ARCHETYPES_FOLDER_NAME.length() + 1, entry.getName().length() - ARCHETYPE_POSTFIX.length());
                    ArchetypeDTO archetypeDTO = new ArchetypeDTOBuilder()
                            .setId(archetypeId)
                            .setSource(src)
                            .createArchetypeDTO();
                    archetypeDTOs.add(archetypeDTO);
                } else if (entry.getName().startsWith(TEMPLATES_FOLDER_NAME) && entry.getName().endsWith(TEMPLATES_POSTFIX)) {
                    String src = IOUtils.toString(zis,"UTF-8");
                    String templateId = entry.getName().substring(TEMPLATES_FOLDER_NAME.length() + 1, entry.getName().length() - TEMPLATES_POSTFIX.length());
                    TemplateDTO templateDTO = new TemplateDTOBuilder()
                            .setId(templateId)
                            .setArcehtypeId(templateId)
                            .setName(templateId)
                            .setSource(src)
                            .createTemplateDTO();
                    templateSourceDTOs.add(templateDTO);
                }else if (entry.getName().startsWith(TERMINOLOGIES_FOLDER_NAME) && entry.getName().endsWith(TERMINOLOGY_POSTFIX)){
                    String src = IOUtils.toString(zis, "UTF-8");
                    String terminologyId = entry.getName().substring(TERMINOLOGIES_FOLDER_NAME.length() + 1, entry.getName().length() - TERMINOLOGY_POSTFIX.length());
                    TerminologyDTO terminologyDTO = new TerminologyDTO();
                    terminologyDTO.setId(terminologyId);
                    terminologyDTO.setSource(src);
                    terminologyDTOs.add(terminologyDTO);
                }else if (entry.getName().startsWith(DSVIEWS_FOLDER_NAME) && entry.getName().endsWith(DSVIEWS_POSTFIX)){
                    String src = IOUtils.toString(zis,"UTF-8");
                    String dsViewId = entry.getName().substring(DSVIEWS_FOLDER_NAME.length() + 1, entry.getName().length()  -  DSVIEWS_POSTFIX.length());
                    DSViewDTOs.add(new DSViewDTO(dsViewId, dsViewId, dsViewId, src));
                }else if (entry.getName().startsWith(GUIDELINES_FOLDER_NAME) && entry.getName().endsWith(GUIDELINES_POSTFIX)){
                    String src = IOUtils.toString(zis,"UTF-8");
                    String guideId = entry.getName().substring(GUIDELINES_FOLDER_NAME.length() + 1, entry.getName().length() - GUIDELINES_POSTFIX.length());
                    guideSourceDTOs.add(new GuideDTO(guideId, src, null, null, Calendar.getInstance().getTime()));
                }else if (entry.getName().startsWith(ARCHETYPE_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    ArchetypeDTO archetypeDTO = (ArchetypeDTO)IOUtils.getObject(IOUtils.toByteArray(zis));
                    archetypeDTOs.add(archetypeDTO);
                    useArchetypeDTOs = true;
                }else if (entry.getName().startsWith(TEMPLATES_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    TemplateDTO templateDTO = (TemplateDTO)IOUtils.getObject(IOUtils.toByteArray(zis));
                    templateDTOs.add(templateDTO);
                    useTemplateDTOs = true;
                }else if (entry.getName().startsWith(GUIDELINES_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    GuideDTO guideDTO = (GuideDTO)IOUtils.getObject(IOUtils.toByteArray(zis));
                    guideDTOs.add(guideDTO);
                    useGuidelineDTOs = true;
                }
            }
        }finally{
            zis.close();
        }
        try {
            if (useArchetypeDTOs){
                ArchetypeManager.getInstance().getArchetypes().registerCMElementsInCache(archetypeDTOs);
            }else{
                ArchetypeManager.getInstance().getArchetypes().registerCMElementsInCache(archetypeSourceDTOs);
            }
            if (useTemplateDTOs){
                ArchetypeManager.getInstance().getTemplates().registerCMElementsInCache(templateDTOs);
            }else{
                ArchetypeManager.getInstance().getTemplates().registerCMElementsInCache(templateSourceDTOs);
            }
            Terminologies.getInstance().registerCMElementsInCache(terminologyDTOs);
            DecisionSupportViews.getInstance().loadDSViews(DSViewDTOs);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
    }
}
