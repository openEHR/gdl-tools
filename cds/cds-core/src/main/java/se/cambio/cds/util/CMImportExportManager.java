package se.cambio.cds.util;

import se.cambio.cds.controller.session.data.Guides;
import se.cambio.cds.controller.session.data.Overviews;
import se.cambio.cds.model.guide.dto.GuideDTO;
import se.cambio.cds.model.overview.dto.OverviewDTO;
import se.cambio.openehr.controller.session.data.Archetypes;
import se.cambio.openehr.controller.session.data.Templates;
import se.cambio.openehr.controller.terminology.session.data.Terminologies;
import se.cambio.openehr.model.archetype.dto.ArchetypeDTO;
import se.cambio.openehr.model.template.dto.TemplateDTO;
import se.cambio.openehr.model.terminology.dto.TerminologyDTO;
import se.cambio.openehr.util.ExceptionHandler;
import se.cambio.openehr.util.IOUtils;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.*;
import java.util.ArrayList;
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
    private static String OVERVIEWS_FOLDER_NAME = "overviews";

    private static String ARCHETYPE_POSTFIX = ".adl";
    private static String TEMPLATES_POSTFIX = ".oet";
    private static String TERMINOLOGY_POSTFIX = ".csv";
    private static String GUIDELINES_POSTFIX = ".gdl";
    private static String OVERVIEWS_POSTFIX = ".dsv";
    private static String DTO_POSTFIX = ".dto";

    private static String ARCHETYPE_PREFIX = ARCHETYPES_FOLDER_NAME+"\\";
    private static String TEMPLATES_PREFIX = TEMPLATES_FOLDER_NAME+"\\";
    private static String TERMINOLOGY_PREFIX = TERMINOLOGIES_FOLDER_NAME+"\\";
    private static String GUIDELINES_PREFIX = GUIDELINES_FOLDER_NAME+"\\";
    private static String OVERVIEWS_PREFIX = OVERVIEWS_FOLDER_NAME+"\\";

    private static String ARCHETYPE_DTO_PREFIX = ARCHETYPES_FOLDER_NAME+DTO_POSTFIX+"\\";
    private static String TEMPLATES_DTO_PREFIX = TEMPLATES_FOLDER_NAME+DTO_POSTFIX+"\\";
    private static String GUIDELINES_DTO_PREFIX = GUIDELINES_FOLDER_NAME+DTO_POSTFIX+"\\";



    public static void exportCurrentCM(File file) throws IOException {
        exportCurrentCM(file, false);
    }

    public static void exportCurrentCM(File file, boolean generateDTOs) throws IOException {

        // out put file
        ZipOutputStream out = new ZipOutputStream(new FileOutputStream(file));
        exportArchetypes(out, generateDTOs);
        exportTemplates(out, generateDTOs);
        exportTerminologies(out);
        exportGuidelines(out, generateDTOs);
        exportOverviews(out);
        out.close();
    }

    public static void exportArchetypes(ZipOutputStream out, boolean generateDTOs) throws IOException {
        for (ArchetypeDTO archetypeDTO: Archetypes.getAllArchetypes()){
            InputStream in = new ByteArrayInputStream(archetypeDTO.getArchetype().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(ARCHETYPE_PREFIX+archetypeDTO.getIdArchetype()+ARCHETYPE_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
            if (generateDTOs){
                out.putNextEntry(new ZipEntry(ARCHETYPE_DTO_PREFIX+archetypeDTO.getIdArchetype()+DTO_POSTFIX));
                in = new ByteArrayInputStream(IOUtils.getBytes(archetypeDTO));
                while ((count = in.read(b)) > 0) {
                    out.write(b, 0, count);
                }
                in.close();
            }
        }
    }

    public static void exportTemplates(ZipOutputStream out, boolean generateDTOs) throws IOException {
        for (TemplateDTO templateDTO: Templates.getAllTemplates()){
            InputStream in = new ByteArrayInputStream(templateDTO.getArchetype().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(TEMPLATES_PREFIX+templateDTO.getIdTemplate()+TEMPLATES_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
            if (generateDTOs){
                out.putNextEntry(new ZipEntry(TEMPLATES_DTO_PREFIX+templateDTO.getIdTemplate()+DTO_POSTFIX));
                in = new ByteArrayInputStream(IOUtils.getBytes(templateDTO));
                while ((count = in.read(b)) > 0) {
                    out.write(b, 0, count);
                }
                in.close();
            }
        }
    }

    public static void exportTerminologies(ZipOutputStream out) throws IOException {
        for (TerminologyDTO terminologyDTO: Terminologies.getAllTerminologies()){
            InputStream in = new ByteArrayInputStream(terminologyDTO.getSrc());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(TERMINOLOGY_PREFIX+terminologyDTO.getTerminologyId()+TERMINOLOGY_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
        }
    }

    public static void exportGuidelines(ZipOutputStream out, boolean generateDTOs) throws IOException {
        for (GuideDTO guideDTO: Guides.getAllGuides()){
            InputStream in = new ByteArrayInputStream(guideDTO.getGuideSrc().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(GUIDELINES_PREFIX+guideDTO.getIdGuide()+GUIDELINES_POSTFIX));
            // buffer size
            byte[] b = new byte[1024];
            int count;
            while ((count = in.read(b)) > 0) {
                out.write(b, 0, count);
            }
            in.close();
            if (generateDTOs){
                out.putNextEntry(new ZipEntry(GUIDELINES_DTO_PREFIX+guideDTO.getIdGuide()+DTO_POSTFIX));
                in = new ByteArrayInputStream(IOUtils.getBytes(guideDTO));
                while ((count = in.read(b)) > 0) {
                    out.write(b, 0, count);
                }
                in.close();
            }
        }
    }

    public static void exportOverviews(ZipOutputStream out) throws IOException {
        for (OverviewDTO overviewDTO: Overviews.getAllOverviews()){
            InputStream in = new ByteArrayInputStream(overviewDTO.getOverviewSrc().getBytes());
            // name the file inside the zip  file
            out.putNextEntry(new ZipEntry(OVERVIEWS_PREFIX+overviewDTO.getIdOverview()+OVERVIEWS_POSTFIX));
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
    public static void importCM(File file) throws IOException {

        Collection<ArchetypeDTO> archetypeDTOs = new ArrayList<ArchetypeDTO>();
        Collection<TemplateDTO> templateDTOs = new ArrayList<TemplateDTO>();
        Collection<TerminologyDTO> terminologyDTOs = new ArrayList<TerminologyDTO>();
        Collection<OverviewDTO> overviewDTOs = new ArrayList<OverviewDTO>();
        Collection<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();

        boolean useArchetypeDTOs = false;
        boolean useTemplateDTOs = false;
        boolean useGuidelineDTOs = false;

        //Look for DTO folders
        ZipInputStream in = new ZipInputStream(new FileInputStream(file));
        try{
            ZipEntry entry;
            while((entry = in.getNextEntry())!=null){
                if (entry.getName().startsWith(ARCHETYPE_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    useArchetypeDTOs = true;
                }else if (entry.getName().startsWith(TEMPLATES_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    useTemplateDTOs = true;
                }else if (entry.getName().startsWith(GUIDELINES_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    useGuidelineDTOs = true;
                }
            }
        }finally{
            // we must always close the zip file.
            in.close();
        }

        in = new ZipInputStream(new FileInputStream(file));
        try{
            byte[] buffer = new byte[2048];
            // now iterate through each item in the stream. The get next
            // entry call will return a ZipEntry for each file in the
            // stream
            ZipEntry entry;
            while((entry = in.getNextEntry())!=null){

                if (entry.getName().startsWith(ARCHETYPES_FOLDER_NAME) && entry.getName().endsWith(ARCHETYPE_POSTFIX)){
                    if (!useArchetypeDTOs){
                        String src = IOUtils.toString(in,"UTF-8");
                        String archetypeId = entry.getName().substring(ARCHETYPES_FOLDER_NAME.length()+1, entry.getName().length()-ARCHETYPE_POSTFIX.length());
                        archetypeDTOs.add(new ArchetypeDTO(archetypeId, archetypeId, archetypeId, null, src, null, null));
                    }
                }else if (entry.getName().startsWith(TEMPLATES_FOLDER_NAME) && entry.getName().endsWith(TEMPLATES_POSTFIX)){
                    if (!useTemplateDTOs){
                        String src = IOUtils.toString(in,"UTF-8");
                        String templateId = entry.getName().substring(TEMPLATES_FOLDER_NAME.length()+1, entry.getName().length()-TEMPLATES_POSTFIX.length());
                        templateDTOs.add(new TemplateDTO(templateId, templateId, templateId, null, null, src, null, null));
                    }
                }else if (entry.getName().startsWith(TERMINOLOGIES_FOLDER_NAME) && entry.getName().endsWith(TERMINOLOGY_POSTFIX)){
                    byte[] src = IOUtils.toByteArray(in);
                    String terminologyId = entry.getName().substring(TERMINOLOGIES_FOLDER_NAME.length()+1, entry.getName().length()-TERMINOLOGY_POSTFIX.length());
                    terminologyDTOs.add(new TerminologyDTO(terminologyId, src));
                }else if (entry.getName().startsWith(OVERVIEWS_FOLDER_NAME) && entry.getName().endsWith(OVERVIEWS_POSTFIX)){
                    String src = IOUtils.toString(in,"UTF-8");
                    String overviewId = entry.getName().substring(OVERVIEWS_FOLDER_NAME.length()+1, entry.getName().length()-OVERVIEWS_POSTFIX.length());
                    overviewDTOs.add(new OverviewDTO(overviewId, overviewId, overviewId, src));
                }else if (entry.getName().startsWith(GUIDELINES_FOLDER_NAME) && entry.getName().endsWith(GUIDELINES_POSTFIX)){
                    if (!useGuidelineDTOs){
                        String src = IOUtils.toString(in,"UTF-8");
                        String guideId = entry.getName().substring(GUIDELINES_FOLDER_NAME.length()+1, entry.getName().length()-GUIDELINES_POSTFIX.length());
                        guideDTOs.add(new GuideDTO(guideId, src, null, null, false));
                    }
                }else if (entry.getName().startsWith(ARCHETYPE_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    ArchetypeDTO archetypeDTO = (ArchetypeDTO)IOUtils.getObject(IOUtils.toByteArray(in));
                    archetypeDTOs.add(archetypeDTO);
                }else if (entry.getName().startsWith(TEMPLATES_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    TemplateDTO templateDTO = (TemplateDTO)IOUtils.getObject(IOUtils.toByteArray(in));
                    templateDTOs.add(templateDTO);
                }else if (entry.getName().startsWith(GUIDELINES_DTO_PREFIX) && entry.getName().endsWith(DTO_POSTFIX)){
                    GuideDTO guideDTO = (GuideDTO)IOUtils.getObject(IOUtils.toByteArray(in));
                    guideDTOs.add(guideDTO);
                }
            }
        }finally{
            // we must always close the zip file.
            in.close();
        }
        try {
            Archetypes.loadArchetypes(archetypeDTOs);
            Templates.loadTemplates(templateDTOs);
            Terminologies.loadTerminologies(terminologyDTOs);
            Guides.loadGuides(guideDTOs);
            Overviews.loadOverviews(overviewDTOs);
        } catch (InternalErrorException e) {
            ExceptionHandler.handle(e);
        }
    }
}
