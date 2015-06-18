package se.cambio.cds.gdl.converters.drools;

import org.joda.time.DateTime;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.openehr.rm.datatypes.text.DvCodedText;
import se.cambio.cds.controller.cds.CDSManager;
import se.cambio.cds.controller.guide.GuideManager;
import se.cambio.cds.model.facade.execution.drools.DroolsRuleExecutionFacadeDelegate;
import se.cambio.cds.model.facade.execution.vo.RuleExecutionResult;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.Domains;
import se.cambio.cds.util.ElementInstanceCollection;
import se.cambio.cm.model.guide.dto.GuideDTO;
import se.cambio.openehr.util.OpenEHRConstUI;
import se.cambio.openehr.util.UserConfigurationManager;
import se.cambio.openehr.util.exceptions.InternalErrorException;
import se.cambio.openehr.util.exceptions.PatientNotFoundException;

import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;

/**
 * User: iago.corbal
 * Date: 2014-06-26
 * Time: 15:21
 */
public class StressTest {

    public static void main(String[] args) {
        try {
            //Load KM
            UserConfigurationManager.setCmFolder(UserConfigurationManager.TERMINOLOGIES_FOLDER_KW, StressTest.class.getClassLoader().getResource("terminologies").toURI().getPath());
            UserConfigurationManager.setCmFolder(UserConfigurationManager.ARCHETYPES_FOLDER_KW, StressTest.class.getClassLoader().getResource("archetypes").toURI().getPath());
            UserConfigurationManager.setCmFolder(UserConfigurationManager.TEMPLATES_FOLDER_KW, StressTest.class.getClassLoader().getResource("templates").toURI().getPath());
            stressTest2();
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }

    private static void stressTest1(){
        Collection<ElementInstance> elementInstances = getGeneratedElementInstancesICD10();
        DroolsRuleExecutionFacadeDelegate droolsREFD = new DroolsRuleExecutionFacadeDelegate();
        List<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();
        try {
            guideDTOs.add(GDLTestCase.parse("CHA2DS2VASc_diagnosis_review.v1"));
            guideDTOs.add(GDLTestCase.parse("Stroke_prevention_dashboard_case.v1"));
            guideDTOs.add(GDLTestCase.parse("MIE_Medication_in_elderly.v1"));

            GuideManager guideManager = new GuideManager(guideDTOs);
            Calendar cal = Calendar.getInstance();
            droolsREFD.execute(null, guideDTOs, new ArrayList<ElementInstance>(), null); //WARM UP THE ENGINE
            long total = 0;
            int numExec = 15;
            for (int i=0;i<numExec;i++){
                ElementInstanceCollection eic = new ElementInstanceCollection();
                eic.addAll(elementInstances);
                CDSManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, cal);
                long startTime = System.currentTimeMillis();
                RuleExecutionResult rer = droolsREFD.execute(null, guideDTOs, eic.getAllElementInstances(), cal);
                long execTime = (System.currentTimeMillis()-startTime);
                System.out.println("Executed in: "+execTime+" ms");
                System.out.println("Rules fired: "+rer.getFiredRules().size());
                total += execTime;
            }
            System.out.println("Executed in: "+(total/numExec)+" ms");
        } catch (InternalErrorException e) {
            e.printStackTrace();
        } catch (PatientNotFoundException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Collection<ElementInstance> getGeneratedElementInstancesICD10(){
        Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
        for (int i = 0; i<3; i++){
            for(String icd10Code: ICD10_CODES){
                ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.DIAGNOSIS_ARCHETYPE_ID, GDLTestCase.DIAGNOSIS_TEMPLATE_ID);
                DataValue dataValue = new DvCodedText(icd10Code, "ICD10", icd10Code);
                ElementInstance eiCode = new ElementInstance(GDLTestCase.DIAGNOSIS_CODE_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
                dataValue = new DvDateTime(new DateTime().toString());
                ElementInstance eiDate = new ElementInstance(GDLTestCase.DIAGNOSIS_DATE_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
                elementInstances.add(eiCode);
                elementInstances.add(eiDate);
            }
        }
        System.out.println("Diagnosis elements generated "+elementInstances.size());
        return elementInstances;
    }

    private static Collection<ElementInstance> getGeneratedElementInstancesATC(){
        Collection<ElementInstance> elementInstances = new ArrayList<ElementInstance>();
        for (int i = 0; i<1; i++){
            for(String atcCode: ATC_CODES){
                ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.MEDICATION_ARCHETYPE_ID, GDLTestCase.MEDICATION_TEMPLATE_ID);
                DataValue dataValue = new DvCodedText(atcCode, "ATC", atcCode);
                ElementInstance eiCode = new ElementInstance(GDLTestCase.MEDICATION_CODE_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
                dataValue = new DvDateTime(new DateTime().plus(-64000000L).toString());
                ElementInstance eiInitDate = new ElementInstance(GDLTestCase.MEDICATION_DATE_INIT_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
                dataValue = null;//new DvDateTime(new DateTime().plus(6400000L).toString());
                ElementInstance eiEndDate = new ElementInstance(GDLTestCase.MEDICATION_DATE_END_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
                elementInstances.add(eiCode);
                elementInstances.add(eiInitDate);
                elementInstances.add(eiEndDate);
            }
        }
        System.out.println("Medication elements generated "+elementInstances.size());
        return elementInstances;
    }

    private static void stressTest2(){
        Collection<ElementInstance> elementInstances = getGeneratedElementInstancesATC();
        ArchetypeReference ar = new ArchetypeReference(Domains.EHR_ID, GDLTestCase.BASIC_DEMOGRAPHICS_ARCHETYPE_ID, null);
        DataValue dataValue = new DvDateTime("1900-01-01T12:00");
        ElementInstance eiBirthdateDate = new ElementInstance(GDLTestCase.BIRTHDATE_DATE_ELEMENT_ID, dataValue, ar, null, dataValue!=null?null: OpenEHRConstUI.NULL_FLAVOUR_CODE_NO_INFO);
        elementInstances.add(eiBirthdateDate);
        DroolsRuleExecutionFacadeDelegate droolsREFD = new DroolsRuleExecutionFacadeDelegate();
        List<GuideDTO> guideDTOs = new ArrayList<GuideDTO>();
        try {
            guideDTOs.add(GDLTestCase.parse("guides/MIE_Medication_in_elderly.v1"));
            GuideManager guideManager = new GuideManager(guideDTOs);
            Calendar cal = Calendar.getInstance();
            droolsREFD.execute(null, guideDTOs, new ArrayList<ElementInstance>(), null); //WARM UP THE ENGINE
            long total = 0;
            int numExec = 15;
            for (int i=0;i<numExec;i++){
                long startTime = System.currentTimeMillis();
                ElementInstanceCollection eic = new ElementInstanceCollection();
                eic.addAll(elementInstances);
                CDSManager.checkForMissingElements(eic, guideManager.getCompleteElementInstanceCollection(), guideManager, cal);
                Collection<ElementInstance> eis = eic.getAllElementInstances();
                RuleExecutionResult rer = droolsREFD.execute(null, guideDTOs, eis, cal);
                long execTime = (System.currentTimeMillis()-startTime);
                System.out.println("Executed in: "+execTime+" ms");
                System.out.println("Rules fired: "+rer.getFiredRules().size());
                total += execTime;
            }
            System.out.println("Avg ("+numExec+") executed in: "+(total/numExec)+" ms");
        } catch (InternalErrorException e) {
            e.printStackTrace();
        } catch (PatientNotFoundException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static String[] ICD10_CODES = new String[]{"I48", "I63", "E12", "F12",
            "A00","A01","A02","A03","A04","A05","A06","A07","A08","A09","A15","A16","A17","A18","A19","A20","A21",
            "A22","A23","A24","A25","A26","A27","A28","A30","A31","A32","A33","A34","A35","A36","A37","A38","A39",
            "A40","A41","A42","A43","A44","A46","A48","A49","A50","A51","A52","A53","A54","A55","A56","A57","A58",
            "A59","A60","A63","A64","A65","A66","A67","A68","A69","A70","A71","A74","A75","A77","A78","A79","A80",
            "A81","A82","A83","A84","A85","A86","A87","A88","A89","A90","A91","A92","A93","A94","A95","A96","A98",
            "A99","B00","B01","B02","B03","B04","B05","B06","B07","B08","B09","B15","B16","B17","B18","B19","B20",
            "B21","B22","B23","B24","B25","B26","B27","B30","B33","B34","B35","B36","B37","B38","B39","B40","B41",
            "B42","B43","B44","B45","B46","B47","B48","B49","B50","B51","B52","B53","B54","B55","B56","B57","B58",
            "B59","B60","B64","B65","B66","B67","B68","B69","B70","B71","B72","B73","B74","B75","B76","B77","B78",
            "B79","B80","B81","B82","B83","B85","B86","B87","B88","B89","B90","B91","B92","B94","B95","B96","B97",
            "B98","B99","C00","C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12","C13","C14",
            "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C30","C31","C32","C33","C34",
            "C37","C38","C39","C40","C41","C43","C44","C45","C46","C47","C48","C49","C50","C51","C52","C53","C54",
            "C55","C56","C57","C58","C60","C61","C62","C63","C64","C65","C66","C67","C68","C69","C70","C71","C72",
            "C73","C74","C75","C76","C77","C78","C79","C80","C81","C82","C83","C84","C85","C86","C88","C90","C91",
            "C92","C93","C94","C95","C96","C97","D00","D01","D02","D03","D04","D05","D06","D07","D09","D10","D11",
            "D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","D22","D23","D24","D25","D26","D27","D28",
            "D29","D30","D31","D32","D33","D34","D35","D36","D37","D38","D39","D40","D41","D42","D43","D44","D45",
            "D46","D47","D48","D50","D51","D52","D53","D55","D56","D57","D58","D59","D60","D61","D62","D63","D64",
            "D65","D66","D67","D68","D69","D70","D71","D72","D73","D74","D75","D76","D77","D80","D81","D82","D83",
            "D84","D86","D89","E00","E01","E02","E03","E04","E05","E06","E07","E10","E11","E12","E13","E14","E15",
            "E16","E20","E21","E22","E23","E24","E25","E26","E27","E28","E29","E30","E31","E32","E34","E35","E40",
            "E41","E42","E43","E44","E45","E46","E50","E51","E52","E53","E54","E55","E56","E58","E59","E60","E61",
            "E63","E64","E65","E66","E67","E68","E70","E71","E72","E73","E74","E75","E76","E77","E78","E79","E80",
            "E83","E84","E85","E86","E87","E88","E89","E90","F00","F01","F02","F03","F04","F05","F06","F07","F09",
            "F10","F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25","F28",
            "F29","F30","F31","F32","F33","F34","F38","F39","F40","F41","F42","F43","F44","F45","F48","F50","F51",
            "F52","F53","F54","F55","F59","F60","F61","F62","F63","F64","F65","F66","F68","F69","F70","F71","F72",
            "F73","F78","F79","F80","F81","F82","F83","F84","F88","F89","F90","F91","F92","F93","F94","F95","F98",
            "F99","G00","G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12","G13","G14","G20",
            "G21","G22","G23","G24","G25","G26","G30","G31","G32","G35","G36","G37","G40","G41","G43","G44","G45",
            "G46","G47","G50","G51","G52","G53","G54","G55","G56","G57","G58","G59","G60","G61","G62","G63","G64",
            "G70","G71","G72","G73","G80","G81","G82","G83","G90","G91","G92","G93","G94","G95","G96","G97","G98",
            "G99","H00","H01","H02","H03","H04","H05","H06","H10","H11","H13","H15","H16","H17","H18","H19","H20",
            "H21","H22","H25","H26","H27","H28","H30","H31","H32","H33","H34","H35","H36","H40","H42","H43","H44",
            "H45","H46","H47","H48","H49","H50","H51","H52","H53","H54","H55","H57","H58","H59","H60","H61","H62",
            "H65","H66","H67","H68","H69","H70","H71","H72","H73","H74","H75","H80","H81","H82","H83","H90","H91",
            "H92","H93","H94","H95","I00","I01","I02","I05","I06","I07","I08","I09","I10","I11","I12","I13","I15",
            "I20","I21","I22","I23","I24","I25","I26","I27","I28","I30","I31","I32","I33","I34","I35","I36","I37",
            "I38","I39","I40","I41","I42","I43","I44","I45","I46","I47","I48","I49","I50","I51","I52","I60","I61",
            "I62","I63","I64","I65","I66","I67","I68","I69","I70","I71","I72","I73","I74","I77","I78","I79","I80",
            "I81","I82","I83","I84","I85","I86","I87","I88","I89","I95","I97","I98","I99","J00","J01","J02","J03",
            "J04","J05","J06","J09","J10","J11","J12","J13","J14","J15","J16","J17","J18","J20","J21","J22","J30",
            "J31","J32","J33","J34","J35","J36","J37","J38","J39","J40","J41","J42","J43","J44","J45","J46","J47",
            "J60","J61","J62","J63","J64","J65","J66","J67","J68","J69","J70","J80","J81","J82","J84","J85","J86",
            "J90","J91","J92","J93","J94","J95","J96","J98","J99","K00","K01","K02","K03","K04","K05","K06","K07",
            "K08","K09","K10","K11","K12","K13","K14","K20","K21","K22","K23","K25","K26","K27","K28","K29","K30",
            "K31","K35","K36","K37","K38","K40","K41","K42","K43","K44","K45","K46","K50","K51","K52","K55","K56",
            "K57","K58","K59","K60","K61","K62","K63","K65","K66","K67","K70","K71","K72","K73","K74","K75","K76",
            "K77","K80","K81","K82","K83","K85","K86","K87","K90","K91","K92","K93","L00","L01","L02","L03","L04",
            "L05","L08","L10","L11","L12","L13","L14","L20","L21","L22","L23","L24","L25","L26","L27","L28","L29",
            "L30","L40","L41","L42","L43","L44","L45","L50","L51","L52","L53","L54","L55","L56","L57","L58","L59",
            "L60","L62","L63","L64","L65","L66","L67","L68","L70","L71","L72","L73","L74","L75","L80","L81","L82",
            "L83","L84","L85","L86","L87","L88","L89","L90","L91","L92","L93","L94","L95","L97","L98","L99","M00",
            "M01","M02","M03","M05","M06","M07","M08","M09","M10","M11","M12","M13","M14","M15","M16","M17","M18",
            "M19","M20","M21","M22","M23","M24","M25","M30","M31","M32","M33","M34","M35","M36","M40","M41","M42",
            "M43","M45","M46","M47","M48","M49","M50","M51","M53","M54","M60","M61","M62","M63","M65","M66","M67",
            "M68","M70","M71","M72","M73","M75","M76","M77","M79","M80","M81","M82","M83","M84","M85","M86","M87",
            "M88","M89","M90","M91","M92","M93","M94","M95","M96","M99","N00","N01","N02","N03","N04","N05","N06",
            "N07","N08","N10","N11","N12","N13","N14","N15","N16","N17","N18","N19","N20","N21","N22","N23","N25",
            "N26","N27","N28","N29","N30","N31","N32","N33","N34","N35","N36","N37","N39","N40","N41","N42","N43",
            "N44","N45","N46","N47","N48","N49","N50","N51","N60","N61","N62","N63","N64","N70","N71","N72","N73",
            "N74","N75","N76","N77","N80","N81","N82","N83","N84","N85","N86","N87","N88","N89","N90","N91","N92",
            "N93","N94","N95","N96","N97","N98","N99","O00","O01","O02","O03","O04","O05","O06","O07","O08","O10",
            "O11","O12","O13","O14","O15","O16","O20","O21","O22","O23","O24","O25","O26","O28","O29","O30","O31",
            "O32","O33","O34","O35","O36","O40","O41","O42","O43","O44","O45","O46","O47","O48","O60","O61","O62",
            "O63","O64","O65","O66","O67","O68","O69","O70","O71","O72","O73","O74","O75","O80","O81","O82","O83",
            "O84","O85","O86","O87","O88","O89","O90","O91","O92","O94","O95","O96","O97","O98","O99","P00","P01",
            "P02","P03","P04","P05","P07","P08","P10","P11","P12","P13","P14","P15","P20","P21","P22","P23","P24",
            "P25","P26","P27","P28","P29","P35","P36","P37","P38","P39","P50","P51","P52","P53","P54","P55","P56",
            "P57","P58","P59","P60","P61","P70","P71","P72","P74","P75","P76","P77","P78","P80","P81","P83","P90",
            "P91","P92","P93","P94","P95","P96","Q00","Q01","Q02","Q03","Q04","Q05","Q06","Q07","Q10","Q11","Q12",
            "Q13","Q14","Q15","Q16","Q17","Q18","Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27","Q28","Q30","Q31",
            "Q32","Q33","Q34","Q35","Q36","Q37","Q38","Q39","Q40","Q41","Q42","Q43","Q44","Q45","Q50","Q51","Q52",
            "Q53","Q54","Q55","Q56","Q60","Q61","Q62","Q63","Q64","Q65","Q66","Q67","Q68","Q69","Q70","Q71","Q72",
            "Q73","Q74","Q75","Q76","Q77","Q78","Q79","Q80","Q81","Q82","Q83","Q84","Q85","Q86","Q87","Q89","Q90",
            "Q91","Q92","Q93","Q95","Q96","Q97","Q970","Q973","Q98","Q980","Q982","Q983","Q985","Q99","Q991","R00","R01",
            "R02","R03","R04","R05","R06","R07","R09","R10","R11","R12","R13","R14","R15","R16","R17","R18","R19",
            "R20","R21","R22","R23","R25","R26","R27","R29","R30","R31","R32","R33","R34","R35","R36","R39","R40",
            "R41","R42","R43","R44","R45","R46","R47","R48","R49","R50","R51","R52","R53","R54","R55","R56","R57",
            "R58","R59","R60","R61","R62","R63","R64","R65","R68","R69","R70","R71","R72","R73","R74","R75","R76",
            "R77","R78","R79","R80","R81","R82","R83","R84","R85","R86","R87","R89","R90","R91","R92","R93","R94",
            "R95","R96","R98","R99","S00","S01","S02","S03","S04","S05","S06","S07","S08","S09","S10","S11","S12",
            "S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24","S25","S26","S27","S28","S29",
            "S30","S31","S32","S33","S34","S35","S36","S37","S38","S39","S40","S41","S42","S43","S44","S45","S46",
            "S47","S48","S49","S50","S51","S52","S53","S54","S55","S56","S57","S58","S59","S60","S61","S62","S63",
            "S64","S65","S66","S67","S68","S69","S70","S71","S72","S73","S74","S75","S76","S77","S78","S79","S80",
            "S81","S82","S83","S84","S85","S86","S87","S88","S89","S90","S91","S92","S93","S94","S95","S96","S97",
            "S98","S99","T00","T01","T02","T03","T04","T05","T06","T07","T08","T09","T10","T11","T12","T13","T14",
            "T15","T16","T17","T18","T19","T20","T21","T22","T23","T24","T25","T26","T27","T28","T29","T30","T31",
            "T32","T33","T34","T35","T36","T37","T38","T39","T40","T41","T42","T43","T44","T45","T46","T47","T48",
            "T49","T50","T51","T52","T53","T54","T55","T56","T57","T58","T59","T60","T61","T62","T63","T64","T65",
            "T66","T67","T68","T69","T70","T71","T73","T74","T75","T78","T79","T80","T81","T82","T83","T84","T85",
            "T86","T87","T88","T90","T91","T92","T93","T94","T95","T96","T97","T98","V01","V02","V03","V04","V05",
            "V06","V09","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24",
            "V25","V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39","V40","V41",
            "V42","V43","V44","V45","V46","V47","V48","V49","V50","V51","V52","V53","V54","V55","V56","V57","V58",
            "V59","V60","V61","V62","V63","V64","V65","V66","V67","V68","V69","V70","V71","V72","V73","V74","V75",
            "V76","V77","V78","V79","V80","V81","V82","V83","V84","V85","V86","V87","V88","V89","V90","V91","V92",
            "V93","V94","V95","V96","V97","V98","V99","W00","W01","W02","W03","W04","W05","W06","W07","W08","W09",
            "W10","W11","W12","W13","W14","W15","W16","W17","W18","W19","W20","W21","W22","W23","W24","W25","W26",
            "W27","W28","W29","W30","W31","W32","W33","W34","W35","W36","W37","W38","W39","W40","W41","W42","W43",
            "W44","W45","W46","W49","W50","W51","W52","W53","W54","W55","W56","W57","W58","W59","W60","W64","W65",
            "W66","W67","W68","W69","W70","W73","W74","W75","W76","W77","W78","W79","W80","W81","W83","W84","W85",
            "W86","W87","W88","W89","W90","W91","W92","W93","W94","W99","X00","X01","X02","X03","X04","X05","X06",
            "X08","X09","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21","X22","X23","X24",
            "X25","X26","X27","X28","X29","X30","X31","X32","X33","X34","X35","X36","X37","X38","X39","X40","X41",
            "X42","X43","X44","X45","X46","X47","X48","X49","X50","X51","X52","X53","X54","X57","X58","X59","X60",
            "X61","X62","X63","X64","X65","X66","X67","X68","X69","X70","X71","X72","X73","X74","X75","X76","X77",
            "X78","X79","X80","X81","X82","X83","X84","X85","X86","X87","X88","X89","X90","X91","X92","X93","X94",
            "X95","X96","X97","X98","X99","Y00","Y01","Y02","Y03","Y04","Y05","Y06","Y07","Y08","Y09","Y10","Y11",
            "Y12","Y13","Y14","Y15","Y16","Y17","Y18","Y19","Y20","Y21","Y22","Y23","Y24","Y25","Y26","Y27","Y28",
            "Y29","Y30","Y31","Y32","Y33","Y34","Y35","Y36","Y40","Y41","Y42","Y43","Y44","Y45","Y46","Y47","Y48",
            "Y49","Y50","Y51","Y52","Y53","Y54","Y55","Y56","Y57","Y58","Y59","Y60","Y61","Y62","Y63","Y64","Y65",
            "Y66","Y69","Y70","Y71","Y72","Y73","Y74","Y75","Y76","Y77","Y78","Y79","Y80","Y81","Y82","Y83","Y84",
            "Y85","Y86","Y87","Y88","Y89","Y90","Y91","Y95","Y96","Y97","Y98","Z00","Z01","Z02","Z03","Z04","Z08",
            "Z09","Z10","Z11","Z12","Z13","Z20","Z21","Z22","Z23","Z24","Z25","Z26","Z27","Z28","Z29","Z30","Z31",
            "Z32","Z33","Z34","Z35","Z36","Z37","Z38","Z39","Z40","Z41","Z42","Z43","Z44","Z45","Z46","Z47","Z48",
            "Z49","Z50","Z51","Z52","Z53","Z54","Z55","Z56","Z57","Z58","Z59","Z60","Z61","Z62","Z63","Z64","Z65"};

    private static String[] ATC_CODES = new String[]{
            "N02AX02","A02","A02B","A02BC","A02BC01","A02BC02","A02BC03","A03","A03A","A03AE","A03AE02",
            "A03AE04","A03F","A03FA","A03FA02","A03FA03","A04","A04A","A04AA","A04AA02","A04AA05","A04AD",
            "A04AD12","A05","A05A","A05AA","A05AA03","A06","A06A","A06AH","A06AH01","A07","A07A",
            "A07AA","A07AA12","A08","A08A","A08AA","A08AA01","A08AA02","A08AA03","A08AA04","A08AA05","A08AA08",
            "A08AA09","A08AA10","A08AB","A08AB01","A08AX","A08AX01","A10","A10A","A10AB","A10AB01","A10AB04",
            "A10AB05","A10AB06","A10AC","A10AC01","A10AD","A10AD01","A10AD05","A10AE","A10AE04","A10AE05","A10AF",
            "A10AF01","A10B","A10BA","A10BA02","A10BB","A10BB12","A10BD","A10BD03","A10BD04","A10BD05","A10BD06",
            "A10BD07","A10BD08","A10BD11","A10BG","A10BG02","A10BG03","A10BH","A10BH01","A10BH02","A10BH03","A10BH05",
            "A10BX","A10BX02","A10BX03","A10BX04","A10BX06","A10BX07","A11","A11H","A11HA","A11HA08","A16",
            "A16A","A16AA","A16AA04","A16AA05","A16AA06","A16AB","A16AB02","A16AB03","A16AB04","A16AB05","A16AB07",
            "A16AB08","A16AB09","A16AB10","A16AX","A16AX03","A16AX04","A16AX05","A16AX06","A16AX07","A16AX08","B",
            "B01","B01A","B01AA03","B01AB","B01AB02","B01AC","B01AC04","B01AC06","B01AC09","B01AC11","B01AC16",
            "B01AC17","B01AC22","B01AC24","B01AC30","B01AD","B01AD02","B01AD07","B01AD08","B01AD10","B01AD11","B01AD12",
            "B01AE","B01AE01","B01AE02","B01AE06","A03BA","B01AX","B01AX05","B01AX06","B02","B02A","B02AA",
            "B02AA02","B02AB","B02AB01","B02B","B02BC","B02BC30","B02BD","B02BD02","B02BD04","B02BD05","B02BD11",
            "B02BX","B02BX04","B02BX05","B03","B03X","B03XA","B03XA01","B03XA02","B03XA03","B06","B06A",
            "B06AC","B06AC01","C","C01","C01B","A03BB","C01BD04","C01BD07","C01E","C01EB","C01EB16",
            "C01EB17","C01EB18","C01EB19","C01EB21","C02","C02C","C02CA","C02CA04","C02K","C02KX","C02KX01",
            "C02KX02","C02KX03","C03","C03X","C03XA","C03XA01","C04","C04A","C04AX","C04AX21","C07",
            "C07A","C07AB","C07AB52","C08","C08C","C08CA","C08CA01","C08CA02","C08CA05","C09","C09A",
            "C09AA","C09AA01","C09AA02","C09AA03","C09AA04","C09AA05","C09AA08","N02AG","C09B","C09BA","C09BA08",
            "C09BB","C09BB03","C09C","C09CA","C09CA01","C09CA03","C09CA04","C09CA07","C09CA09","C09D","C09DA",
            /*"C09DA04","C09DA06","C09DA07","C09DB","C09DB01","C09DB04","C09DX","C09DX01","C09X","C09XA","C09XA02",
            "C09XA52","C09XA53","C09XA54","C10","C10A","C10AA","C10AA01","C10AA03","C10AA04","C10AA05","C10AA06",
            "C10AA07","C10AB","C10AB04","A04AD01","C10AC04","C10AD","C10AD52","C10AX","C10B","C10BA","C10BA03",
            "D","D01","N05AB04","D01AC","D01AC15","D03","D03A","N04A","D03AX06","D05","D05A",
            "D05AX","D05AX52","D06","D06A","D06AX","N05AH02","D06B","D06BB","D06BB10","D10","D10A",
            "D10AD","D10AD04","D11","D11A","D11AH","D11AH01","D11AH02","D11AX","D11AX16","G","G02",
            "G02C","G02CB","G02CB02","G02CX","G02CX01","G03","G03A","G03AA","G03AA14","G03AC","G03AC08",
            "G03B","G03BA","G03BA03","G03F","G03FA","G03FA01","G03G","G03GA","G03GA01","G03GA05","G03GA06",
            "G03GA07","G03GA09","G03X","G03XB","G03XB01","G03XC","G03XC01","G03XC02","G03XC03","G04","G04B",
            "G04BD","G04BD04","G04BD10","G04BD11","G04BE","G04BE03","G04BE07","G04BE08","G04BE09","G04C","G04CA",
            "G04CA04","H","H01","H01A","H01AB","H01AB01","H01AC","H01AC01","H01AC03","H01AX","H01AX01",
            "H01C","H01CA","H01CA03","H01CB","H01CB05","H01CC","H01CC01","H02","H02A","H02AB","H02AB02",
            "H02AB09","H05","H05A","H05AA","H05AA02","H05AA03","H05B","H05BA","H05BA01","H05BX","H05BX01",
            "J","J01","J01A","J01AA","J01AA12","J01C","J01CE","N05BB01","J01CR","J01CR02","J01CR05",
            "J01D","J01DC","J01DC02","J01DD","J01DD02","J01DD04","J01DF","J01DF01","J01DH","J01DH02","J01DH03",
            "J01DH04","J01DH51","J01DI","J01DI01","J01DI02","J01F","J01FA","J01FA15","J01G","J01GB","J01GB01",
            "J01M","J01MA","J01MA02","J01MA09","J01MA13","J01MA14","J01MA16","J01X","J01XA","J01XA02","J01XA03",
            "J01XX","J01XX09","J02","J02A","R06AB","J02AC01","J02AC03","J02AC04","J02AX","J02AX04","J02AX05",
            "J02AX06","J05","J05A","J05AB","J05AB01","J05AB04","J05AB06","J05AB09","J05AB11","J05AB12","J05AE",
            "J05AE01","J05AE02","J05AE03","J05AE04","J05AE05","J05AE06","R06AX02","J05AE08","J05AE09","J05AE10","J05AF",
            "J05AF04","J05AF05","J05AF06","J05AF07","J05AF08","J05AF09","J05AF10","J05AF11","J05AG","J05AG01","J05AG03",
            "J05AG04","J05AG05","J05AH","J05AH02","J05AR","J05AR01","J05AR02","J05AR03","J05AR06","J05AR08","J05AX",
            "J05AX07","J05AX08","J05AX09","J06","J06B","J06BA","J06BA01","J06BA02","J06BB","J06BB04","J06BB16",
            "J07","J07A","J07AE","J07AE01","J07AH","J07AH08","J07AL","J07AL02","J07AL52","J07B","J07BA",
            "J07BA02","J07BB","J07BB01","J07BB02","J07BB03","J07BC","J07BC01","J07BC20","J07BD","J07BD52","J07BD54",
            "J07BH","J07BH01","J07BH02","J07BK","J07BK02","J07BL","J07BL01","J07BM","J07BM01",
            "J07BM02","J07C","J07CA","J07CA01","J07CA05","J07CA07","J07CA08","J07CA09","J07CA10","J07CA12","M",
            "M01","M01A","M01AC","M01AC01","M01AC05","M01AE","M01AE03","M01AH","M01AH02","M01AH03","M01AH04",
            "M01AH05","M01AH06","M01AX","M01AX05","M01AX17","M03","M03A","M03AX","M03AX01","M03B","M03BA",
            "M03BA02","M03BB","M03BB02","M03BX","M03BX04","M04","M04A","M04AA","M04AA03","M05","M05B",
            "M05BA","M05BA04","M05BA06","M05BA08","M05BB","M05BB03","M05BC","M05BC01","M05BC02","M05BX","M05BX03",
            "M05BX04","M09","M09A","M09AB","M09AB02","M09AX","M09AX02","N","N01","N01A","N01AH",
            "N01AH01","N01B","N01BX","N01BX04","N02","N02A","N02AB","N02AB03","N02AX","N02AX02","N02B",
            "N02BG","N02BG08","N02C","N02CX","N02CX04","N03","N03A","N03AF","N03AF03","N03AF04","N03AG",
            "N03AG04","N03AX","N03AX09","N03AX11","N03AX12","N03AX14","N03AX15","N03AX16","N03AX17","N03AX18","N03AX21",
            "N03AX22","N04","N04B","N04BA","N04BA03","N04BC","N04BC04","N04BC05","N04BC09","N04BD","N04BD02",
            "N04BX","N04BX01","N04BX02","N05","N05A","N05AE","N05AE03","N05AH","N05AH02","N05AH03","N05AH04",
            "N05AH05","N05AL","N05AL06","N05AX","N05AX08","N05AX12","N05AX13","N05C","N05CD","N05CD08","N05CF",
            "N05CF03","N05CH","N05CH01","N05CM","N05CM18","N06","N06A","N06AB","N06AB03","N06AB04","N06AB05",
            "N06AB06","N06AB08","N06AB10","N06AX","N06AX11","N06AX12","N06AX16","N06AX17","N06AX18","N06AX21","N06AX22",
            "N06B","N06BA","N06BA04","N06BA09","N06BC","N06BC01","N06BC02","N06D","N06DA","N06DA03","N06DX",
            "N06DX01","N07","N07B","N07BA","N07BA03","N07BC","N07BC03","N07BC51","N07C","N07CA","N07CA01",
            "N07X","N07XX","N07XX02","N07XX04","N07XX05","N07XX07","N07XX08","P","P01","P01B","P01BF",
            "P01BF05","R","R01","R01A","R01AD","R01AD12","R03","R03A","R03AC","R03AC02","R03AC13",
            "R03AC18","R03AK","R03AK06","R03B","R03BA","R03BA02","R03BA08","R03BB","R03BB05","R03D","R03DC",
            "R03DC03","R03DX","R03DX05","R03DX07","R05","R05C","R05CB","R05CB16","R05D","R05DA","R05DA08",
            "R05DB","R05DB03","R06","R06A","R06AE","R06AE07","R06AX","R06AX12","R06AX13","R06AX25","R06AX27",
            "R07","R07A","R07AX","R07AX01","S","S01","S01A","S01AD","S01AD08","S01AX","S01AX12",
            "S01B","S01BA","S01BA01","S01BC","S01BC10","S01BC11","S01E","S01EC","S01EC04","S01ED","S01ED51",
            "S01EE","S01EE01","S01EE03","S01EE04","S01G","S01GX","S01GX06","S01GX09","S01L","S01LA","S01LA01",
            "S01LA03","S01LA04","V","V03","V03A","V03AB","V03AB17","V03AB33","V03AB35","V03AC","V03AC02"*/};
}
