package se.cambio.openehr.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.*;

/**
 * User: iago.corbal
 * Date: 2014-01-28
 * Time: 11:32
 */
public class SNOMEDCTIsAParser {

    public static void main(String[] args){
        try {
            BufferedReader br = new BufferedReader(new FileReader("sct2_Relationship_Full_INT_20130731.txt"));
            Map<String,String> parentMap = new HashMap<String, String>();
            System.out.println("Creating parent map...");
            String line;
            while ((line = br.readLine()) != null) {
                String[] elements = line.split("\t");
                if (elements[7].equals("116680003")){ //is_a relationship
                    parentMap.put(elements[4], elements[5]);
                }
            }
            System.out.println("Parent map created");
            br.close();


            BufferedWriter bw = new BufferedWriter(new FileWriter("SNOMEDCT.csv"));
            bw.write("id,text,parent\n");
            System.out.println("Creating SNOMED CT terminology file...");
            br = new BufferedReader(new FileReader("C:\\Users\\iago.corbal\\Desktop\\sct2_Description_Full-en_INT_20130731.txt"));
            br.readLine(); //Remove header
            Set<String> idsProccessed = new HashSet<String>();
            while ((line = br.readLine()) != null) {
                String[] elements = line.split("\t");
                if (!idsProccessed.contains(elements[4]) && elements[6].equals("900000000000003001")){ //Fully specified name
                    bw.write(elements[4]);
                    bw.write(",");
                    bw.write("\""+elements[7]+"\"");
                    bw.write(",");
                    String parent = parentMap.get(elements[4]);
                    if (parent!=null){
                        bw.write(parent);
                    }
                    bw.write("\n");
                    idsProccessed.add(elements[4]);
                }
            }
            br.close();
            bw.close();
            System.out.println("SNOMED CT terminology file created successfully!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
