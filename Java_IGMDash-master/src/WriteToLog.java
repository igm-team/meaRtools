
import java.io.FileWriter;
import java.io.IOException;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author dh2744
 */
public class WriteToLog {
    public static int writeToLog(String logFileParentString, javax.swing.JTextArea outputTextArea) {
        int exitValue = 0;
        try {
            // get outputText are
            String logStore = outputTextArea.getText();
            System.out.println( logStore );
                
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd-hh_mm_ss");
            System.out.println(" df "+df);
            Date dateobj = new Date();
            System.out.println(" dateobj "+ dateobj );
            String dateString1=dateobj.toString();
            String dateString= df.format(dateobj);
            System.out.println("dateString "+ dateString );
            
            //check that root directory exists
            String logFileString= 
                     logFileParentString.concat( "LogFile_"+dateString+".txt"  );
            System.out.println( "log file: " + logFileString );
            
            
            FileWriter f = null;
            try {
 
                f = new FileWriter( logFileString );
                f.write(logStore);

                f.write("\n");
                f.close();
             } catch (IOException ex) {
                System.out.println("error in opening log file");
                Logger.getLogger(IGM_MEA_main.class.getName()).log(Level.SEVERE, null, ex);
            }
            
        } catch (Exception e) {
            exitValue = -1;
            //print some error message
        }
        return (exitValue);
    }
    
}
