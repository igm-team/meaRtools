
import java.io.File;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author dh2744
 */
public class RemoveSqBrackets {
     public static String[] removeSqBrackets(javax.swing.JTextField spikeListField)
        {
        // this check whether files actually exist
        int exitValue = 0;
        try{
            String check = spikeListField.getText();
            String[] temp = check.split(",");

            String[] spkCsvFile = temp;
            if (temp.length > 1) {
                for (int ind = 0; ind < temp.length; ind++) {

                    if (0 == ind) {
                        spkCsvFile[ind] = temp[ind].substring(1, temp[ind].length()).trim();
                        System.out.println("spkCsvFile[ind] " + spkCsvFile[ind]);
                        File fileCheck = new File(spkCsvFile[ind]);
                        System.out.println("exists? spkCsvFile[" + ind + "] " + fileCheck.exists());
                    } else if (ind == (temp.length - 1)) {
                        spkCsvFile[ind] = temp[ind].substring(0, temp[ind].length() - 1).trim();
                        System.out.println("spkCsvFile[ind] " + spkCsvFile[ind]);
                        File fileCheck = new File(spkCsvFile[ind]);
                        System.out.println("exists? spkCsvFile[" + ind + "] " + fileCheck.exists());

                    } else {
                        spkCsvFile[ind] = temp[ind].trim();
                        System.out.println("spkCsvFile[ind] " + spkCsvFile[ind]);
                        File fileCheck = new File(spkCsvFile[ind].trim());
                        System.out.println("exists? spkCsvFile[" + ind + "] " + fileCheck.exists());
                    }

                } //end of for loop
            } else {
                int ind = 0;
                int len2 = temp[ind].length();
                len2--;
                spkCsvFile[ind] = temp[ind].substring(1, len2).trim();
                System.out.println("spkCsvFile[ind] " + spkCsvFile[ind]);
                File fileCheck = new File(spkCsvFile[ind]);
                System.out.println("exists? spkCsvFile[" + ind + "] " + fileCheck.exists());
            }

            return spkCsvFile ;
            
            
        } catch (Exception e ) {
            String[]  blank={"-"} ;

            return  blank ;
            
        }
        
    }
    
}
